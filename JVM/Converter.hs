{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Functions to convert from low-level .class format representation and
-- high-level Java classes, methods etc representation
module JVM.Converter
  (parseClass, parseClassFile,
   classFile2Direct, classDirect2File,
   encodeClass,
   methodByName,
   attrByName,
   methodCode
  )
  where

import Data.Maybe (catMaybes)
import           Control.Monad              (when, unless, forM)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.State.Strict (State, execState, state, gets, modify)
import           Data.Binary
import           Control.Applicative ((<|>))
import           Data.Bits
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 ()
import           Data.Default               ()
import           Data.List
import qualified Data.Bimap                 as Bimap
import           Data.Bimap                 ((!))
import qualified Data.Map                   as M
import qualified Data.Set                   as S

import           JVM.ClassFile
import           JVM.Common
import           JVM.Exceptions

-- | Parse .class file data
parseClass :: B.ByteString -> Class Direct
parseClass bstr = classFile2Direct $ decode bstr

-- | Parse class data from file
parseClassFile :: FilePath -> IO (Class Direct)
parseClassFile path = classFile2Direct `fmap` decodeFile path

encodeClass :: Class Direct -> B.ByteString
encodeClass cls = encode $ classDirect2File cls

classFile2Direct :: Class File -> Class Direct
classFile2Direct Class {..} =
  let pool = poolFile2Direct constsPool
      superName = className $ pool ! superClass
      d = defaultClass :: Class Direct
  in d {
      constsPoolSize = fromIntegral (Bimap.size pool),
      constsPool = pool,
      accessFlags = accessFile2Direct accessFlags,
      thisClass = className $ pool ! thisClass,
      superClass = if superClass == 0 then "" else superName,
      interfacesCount = interfacesCount,
      interfaces = map (\i -> className $ pool ! i) interfaces,
      classFieldsCount = classFieldsCount,
      classFields = map (fieldFile2Direct pool) classFields,
      classMethodsCount = classMethodsCount,
      classMethods = map (methodFile2Direct pool) classMethods,
      classAttributesCount = classAttributesCount,
      classAttributes = attributesFile2Direct pool classAttributes }

classDirect2File :: Class Direct -> Class File
classDirect2File Class {..} =
  let d = defaultClass :: Class File
  in d {
    constsPoolSize = fromIntegral (Bimap.size poolInfo + 1),
    constsPool = poolInfo,
    accessFlags = accessDirect2File accessFlags,
    thisClass = force "this" $ poolClassIndex poolInfo thisClass,
    superClass = force "super" $ poolClassIndex poolInfo superClass,
    interfacesCount = fromIntegral (length interfaces),
    interfaces = map (force "ifaces" . poolClassIndex poolInfo) interfaces,
    classFieldsCount = fromIntegral (length classFields),
    classFields = map (fieldDirect2File poolInfo) classFields,
    classMethodsCount = fromIntegral (length classMethods),
    classMethods = map (methodDirect2File poolInfo) classMethods,
    classAttributesCount = fromIntegral $ arsize classAttributes,
    classAttributes = to (arlist classAttributes) }
  where
    poolInfo = poolDirect2File constsPool
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo poolInfo) pairs)

poolDirect2File :: Pool Direct -> Pool File
poolDirect2File directPool = snd $ execState comp (directPool, Bimap.empty)
    where
        -- State is the previous pool of constants to translate, the pool of pending constants, and the in-progress file
        -- pool.
        comp :: State (Pool Direct, Pool File) ()
        comp = do
            curr <- gets fst
            -- Iterate over all the items in the pending pool, if all their dependencies are ready then add them to the
            -- result pool. Otherwise, gather them into `next` and repeat
            next <- forM (Bimap.toList curr) $ \(i, c) -> do
                let addToPool c' = state (\(x, y) -> (Nothing, (x, Bimap.insert i c' y)))
                    handleIndex Left{} = return $ Just (i, c) -- If we couldn't find an index, we'll try next iteration
                    handleIndex (Right c') = addToPool c' -- If we found all the indices
                pool <- gets snd
                case c of
                    CClass name -> handleIndex $ CClass <$> poolIndex pool name
                    CField cls name -> handleIndex $ CField <$> poolClassIndex pool cls <*> poolNTIndex pool name
                    CMethod cls name -> handleIndex $ CMethod <$> poolClassIndex pool cls <*> poolNTIndex pool name
                    CIfaceMethod cls name -> handleIndex $ CIfaceMethod <$> poolIndex pool cls <*> poolNTIndex pool name
                    CString s -> handleIndex $ CString <$> poolIndex pool s
                    CInteger x -> addToPool $ CInteger x
                    CFloat x -> addToPool $ CFloat x
                    CLong x -> addToPool $ CLong (fromIntegral x)
                    CDouble x -> addToPool $ CDouble x
                    CNameType n t -> handleIndex $ CNameType <$> poolIndex pool n <*> poolIndex pool t
                    CUTF8 s -> addToPool $ CUTF8 s
                    CUnicode s -> addToPool $ CUnicode s
                    CMethodHandle t cls b ->
                        handleIndex $ CMethodHandle t <$> poolClassIndex pool cls <*> poolMethodIndex pool cls b
                    CMethodType b -> handleIndex $ CMethodType <$> poolIndex pool b
                    CInvokeDynamic t b -> handleIndex $ CInvokeDynamic t <$> poolNTIndex pool b
            unless (null next) $ do
                let next' = Bimap.fromList $ catMaybes next
                -- Replace the previous/current pools with the ones we just calculated
                when (curr == next') $ error "Encountered infinite loop in poolDirect2File"
                modify (\(_,x) -> (next',x))
                comp

-- | Find index of given string in the list of constants
poolIndex :: MonadError GeneratorException m => Pool File -> B.ByteString -> m Word16
poolIndex pool name = case Bimap.lookupR (CUTF8 name) pool <|> Bimap.lookupR (CUnicode name) pool of
    Nothing -> throwError (NoItemInPool name)
    Just i  ->  return $ fromIntegral i

-- | Find index of given string in the list of constants
poolClassIndex :: MonadError GeneratorException m => Pool File -> B.ByteString -> m Word16
poolClassIndex pool name = do
    i <- poolIndex pool name
    case Bimap.lookupR (CClass $ fromIntegral i) pool of
        Nothing -> throwError (NoItemInPool i)
        Just j  -> return $ fromIntegral j

poolNTIndex :: (MonadError GeneratorException m, HasSignature a) => Pool File -> NameType a -> m Word16
poolNTIndex pool nt = do
    ni <- poolIndex pool (ntName nt)
    ti <- poolIndex pool (byteString $ ntSignature nt)
    case Bimap.lookupR (CNameType ni ti) pool of
        Nothing -> throwError (NoItemInPool nt)
        Just i  -> return $ fromIntegral i

poolMethodIndex :: MonadError GeneratorException m => Pool File -> B.ByteString -> Method Direct -> m Word16
poolMethodIndex pool cls m = do
    si <- poolNTIndex pool (methodNameType m)
    ci <- poolClassIndex pool cls
    case Bimap.lookupR (CMethod ci si) pool of
        Nothing -> throwError (NoItemInPool m)
        Just i  -> return $ fromIntegral i

fieldDirect2File :: Pool File -> Field Direct -> Field File
fieldDirect2File pool Field {..} = Field {
    fieldAccessFlags = accessDirect2File fieldAccessFlags,
    fieldName = force "field name" $ poolIndex pool fieldName,
    fieldSignature = force "signature" $ poolIndex pool (encode fieldSignature),
    fieldAttributesCount = fromIntegral (arsize fieldAttributes),
    fieldAttributes = to (arlist fieldAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

methodDirect2File :: Pool File -> Method Direct -> Method File
methodDirect2File pool Method {..} = Method {
    methodAccessFlags = accessDirect2File methodAccessFlags,
    methodName = force "method name" $ poolIndex pool methodName,
    methodSignature = force "method sig" $ poolIndex pool (encode methodSignature),
    methodAttributesCount = fromIntegral (arsize methodAttributes),
    methodAttributes = to (arlist methodAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

attrInfo :: Pool File -> (B.ByteString, B.ByteString) -> Attribute
attrInfo pool (name, value) = Attribute {
  attributeName = force "attr name" $ poolIndex pool name,
  attributeLength = fromIntegral (B.length value),
  attributeValue = value }

poolFile2Direct :: Pool File -> Pool Direct
poolFile2Direct ps = pool
  where
    pool :: Pool Direct
    pool = bimapMap convert ps

    convertNameType :: HasSignature a => Word16 -> NameType a
    convertNameType i = case pool ! i of
        CNameType n s -> NameType n (decode s)
        _             -> error $ "Unexpected: " ++ show i

    convert (CClass i) = case pool ! i of
                          CUTF8 name -> CClass name
                          x          -> error $ "Unexpected class name: " ++ show x ++ " at " ++ show i
    convert (CField i j) = CField (className $ pool ! i) (convertNameType j)
    convert (CMethod i j) = CMethod (className $ pool ! i) (convertNameType j)
    convert (CIfaceMethod i j) = CIfaceMethod (className $ pool ! i) (convertNameType j)
    convert (CString i) = CString $ getString $ pool ! i
    convert (CInteger x) = CInteger x
    convert (CFloat x)   = CFloat x
    convert (CLong x)    = CLong (fromIntegral x)
    convert (CDouble x)  = CDouble x
    convert (CNameType i j) = CNameType (getString $ pool ! i) (getString $ pool ! j)
    convert (CUTF8 bs) = CUTF8 bs
    convert (CUnicode bs) = CUnicode bs
    convert (CMethodHandle _ _ _) = error "Idk can't figure this out"
    convert (CMethodType i) = CMethodType (getString $ pool ! i)
    convert (CInvokeDynamic t i) = CInvokeDynamic t (convertNameType i)

accessFile2Direct :: AccessFlags File -> AccessFlags Direct
accessFile2Direct w = S.fromList $ concat $ zipWith (\i f -> if testBit w i then [f] else []) [0..] $ [
   ACC_PUBLIC,
   ACC_PRIVATE,
   ACC_PROTECTED,
   ACC_STATIC,
   ACC_FINAL,
   ACC_SYNCHRONIZED,
   ACC_VOLATILE,
   ACC_TRANSIENT,
   ACC_NATIVE,
   ACC_INTERFACE,
   ACC_ABSTRACT ]

accessDirect2File :: AccessFlags Direct -> AccessFlags File
accessDirect2File fs = bitsOr $ map toBit $ S.toList fs
  where
    bitsOr = foldl (.|.) 0
    toBit f = 1 `shiftL` (fromIntegral $ fromEnum f)

fieldFile2Direct :: Pool Direct -> Field File -> Field Direct
fieldFile2Direct pool Field {..} = Field {
  fieldAccessFlags = accessFile2Direct fieldAccessFlags,
  fieldName = getString $ pool ! fieldName,
  fieldSignature = decode $ getString $ pool ! fieldSignature,
  fieldAttributesCount = fromIntegral (apsize fieldAttributes),
  fieldAttributes = attributesFile2Direct pool fieldAttributes }

methodFile2Direct :: Pool Direct -> Method File -> Method Direct
methodFile2Direct pool Method {..} = Method {
  methodAccessFlags = accessFile2Direct methodAccessFlags,
  methodName = getString $ pool ! methodName,
  methodSignature = decode $ getString $ pool ! methodSignature,
  methodAttributesCount = fromIntegral (apsize methodAttributes),
  methodAttributes = attributesFile2Direct pool methodAttributes }

attributesFile2Direct :: Pool Direct -> Attributes File -> Attributes Direct
attributesFile2Direct pool (AP attrs) = AR (M.fromList $ map go attrs)
  where
    go :: Attribute -> (B.ByteString, B.ByteString)
    go Attribute {..} = (getString $ pool ! attributeName,
                           attributeValue)

-- | Try to get class method by name
methodByName :: Class Direct -> B.ByteString -> Maybe (Method Direct)
methodByName cls name =
  find (\m -> methodName m == name) (classMethods cls)

-- | Try to get object attribute by name
attrByName :: (HasAttributes a) => a Direct -> B.ByteString -> Maybe B.ByteString
attrByName x name =
  let (AR m) = attributes x
  in  M.lookup name m

-- | Try to get Code for class method (no Code for interface methods)
methodCode :: Class Direct
           -> B.ByteString       -- ^ Method name
           -> Maybe B.ByteString
methodCode cls name = do
  method <- methodByName cls name
  attrByName method "Code"

