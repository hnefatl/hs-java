{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- | This module defines Generate[IO] monad, which helps generating JVM code and
-- creating Java class constants pool.
--
-- Code generation could be done using one of two monads: Generate and GenerateIO.
-- Generate monad is pure (simply State monad), while GenerateIO is IO-related.
-- In GenerateIO additional actions are available, such as setting up ClassPath
-- and loading classes (from .class files or JAR archives).
--
module JVM.Builder.Monad
  (GState (..),
   emptyGState,
   -- GeneratorMonad (..),
   -- Generator (..),
   Generate (..), -- GenerateIO (..),
   addToPool,
   i0, i1, i8,
   newMethod,
   setStackSize, setMaxLocals,
   withClassPath,
   getClassField, getClassMethod,
   generate, generateIO,
   generateCodeLength
  ) where

import Prelude hiding (catch)
import Control.Monad.State as St
import Data.Word
import Data.Binary
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Control.Exception.Safe.Checked
import Control.Monad.Except

import JVM.Common
import JVM.ClassFile
import JVM.Assembler
import JVM.Exceptions
import Java.ClassPath
import JVM.AccessFlag

-- | Generator state
data GState = GState {
  generated :: [Instruction],             -- ^ Already generated code (in current method)
  currentPool :: Pool Direct,             -- ^ Already generated constants pool
  nextPoolIndex :: Word16,                -- ^ Next index to be used in constants pool
  doneMethods :: [Method Direct],         -- ^ Already generated class methods
  currentMethod :: Maybe (Method Direct), -- ^ Current method
  stackSize :: Word16,                    -- ^ Maximum stack size for current method
  locals :: Word16,                       -- ^ Maximum number of local variables for current method
  classPath :: [Tree CPEntry]
  }
  deriving (Eq,Show)

-- | Empty generator state
emptyGState ::  GState
emptyGState = GState {
  generated = [],
  currentPool = M.empty,
  nextPoolIndex = 1,
  doneMethods = [],
  currentMethod = Nothing,
  stackSize = 496,
  locals = 0,
  classPath = []}

modifyGState :: (Monad m, MonadState GState m) => (GState -> GState) -> m ()
modifyGState fn = do
  st <- St.get
  St.put $ fn st

getsGState :: (Monad m, MonadState GState m) => (GState -> a) -> m a
getsGState fn = do
  st <- St.get
  return $ fn st

-- | Generate monad
newtype Generate e m a = Generate {
  runGenerate :: ExceptT e (StateT GState m) a }
  deriving (Functor, Applicative, Monad, MonadState GState, MonadThrow, MonadCatch, MonadIO)

execGenerateIO :: Monad m => [Tree CPEntry] -> Generate e m a -> m GState
execGenerateIO cp (Generate emt) = do
    execStateT (runExceptT emt) (emptyGState {classPath = cp})

execGenerate :: [Tree CPEntry]
                -> Generate e Identity a
                -> GState
execGenerate cp g =
    runIdentity $ execGenerateIO cp g

-- | Update ClassPath
withClassPath :: (MonadIO m, MonadState GState m) => ClassPath () -> m ()
withClassPath cp = do
  res <- liftIO $ execClassPath cp
  st <- St.get
  St.put $ st {classPath = res}

-- | Add a constant to pool
addItem :: (Monad m, MonadState GState m) => Constant Direct -> m Word16
addItem c = do
  pool <- getsGState currentPool
  case lookupPool c pool of
    Just i -> return i
    Nothing -> do
      i <- getsGState nextPoolIndex
      let pool' = M.insert i c pool
          i' = if long c
                 then i+2
                 else i+1
      modifyGState $ \st ->
            st {currentPool = pool',
                nextPoolIndex = i'}
      return i

-- | Lookup in a pool
lookupPool :: Constant Direct -> Pool Direct -> Maybe Word16
lookupPool c pool =
  fromIntegral `fmap` mapFindIndex (== c) pool

addNT :: (Monad m, MonadState GState m, HasSignature a) => NameType a -> m Word16
addNT (NameType name sig) = do
  let bsig = encode sig
  x <- addItem (CNameType name bsig)
  addItem (CUTF8 name)
  addItem (CUTF8 bsig)
  return x

addSig :: (Monad m, MonadState GState m) => MethodSignature -> m Word16
addSig c@(MethodSignature args ret) = do
  let bsig = encode c
  addItem (CUTF8 bsig)

-- | Add a constant into pool
addToPool :: (Monad m, MonadState GState m) => Constant Direct -> m Word16
addToPool c@(CClass str) = do
  addItem (CUTF8 str)
  addItem c
addToPool c@(CField cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CIfaceMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CString str) = do
  addToPool (CUTF8 str)
  addItem c
addToPool c@(CNameType name sig) = do
  addItem (CUTF8 name)
  addItem (CUTF8 sig)
  addItem c
addToPool c = addItem c

putInstruction :: (Monad m, MonadState GState m) => Instruction -> m ()
putInstruction instr = do
  modifyGState $ \st -> st {generated = generated st ++ [instr]}

-- | Generate one (zero-arguments) instruction
i0 :: (Monad m, MonadState GState m) => Instruction -> m ()
i0 = putInstruction

-- | Generate one one-argument instruction
i1 :: (Monad m, MonadState GState m) => (Word16 -> Instruction) -> Constant Direct -> m ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

-- | Generate one one-argument instruction
i8 :: (Monad m, MonadState GState m) => (Word8 -> Instruction) -> Constant Direct -> m ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

-- | Set maximum stack size for current method
setStackSize :: (Monad m, MonadState GState m) => Word16 -> m ()
setStackSize n = do
  modifyGState $ \st -> st {stackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: (Monad m, MonadState GState m) => Word16 -> m ()
setMaxLocals n = do
  modifyGState $ \st -> st {locals = n}

-- | Start generating new method
startMethod :: (Monad m, MonadState GState m) => [MethodAccessFlag] -> B.ByteString -> MethodSignature -> m ()
startMethod flags name sig = do
  addToPool (CString name)
  addSig sig
  setStackSize 4096
  setMaxLocals 100
  st <- St.get
  let method = Method {
    methodAccessFlags = S.fromList flags,
    methodName = name,
    methodSignature = sig,
    methodAttributesCount = 0,
    methodAttributes = AR [] }
  St.put $ st {generated = [],
               currentMethod = Just method }

-- | End of method generation
endMethod :: (Monad m, MonadState GState m, MonadThrow m, Throws UnexpectedEndMethod) => m ()
endMethod = do
  m <- getsGState currentMethod
  code <- getsGState genCode
  case m of
    Nothing -> throw UnexpectedEndMethod
    Just method -> do
      let method' = method {methodAttributes = AR $ [("Code", encodeMethod code)],
                            methodAttributesCount = 1}
      modifyGState $ \st ->
               st {generated = [],
                   currentMethod = Nothing,
                   doneMethods = doneMethods st ++ [method']}

-- | Generate new method
newMethod :: (Monad m, MonadState GState m, MonadThrow m, Throws UnexpectedEndMethod)
          => [MethodAccessFlag]        -- ^ Access flags for method (public, static etc)
          -> B.ByteString        -- ^ Method name
          -> [ArgumentSignature] -- ^ Signatures of method arguments
          -> ReturnSignature     -- ^ Method return signature
          -> m ()                -- ^ Generator for method code
          -> m (NameType (Method Direct))
newMethod flags name args ret gen = do
  let sig = MethodSignature args ret
  startMethod flags name sig
  gen
  endMethod
  return (NameType name sig)

-- | Get a class from current ClassPath
getClass :: (Throws ENotLoaded, Throws ENotFound, MonadThrow m,MonadState GState m, MonadIO m)
         => String -> m (Class Direct)
getClass name = do
  cp <- classPath <$> St.get
  res <- liftIO $ getEntry cp name
  case res of
    Just (NotLoaded p) -> throw (ClassFileNotLoaded p)
    Just (Loaded _ c) -> return c
    Just (NotLoadedJAR p c) -> throw (JARNotLoaded p c)
    Just (LoadedJAR _ c) -> return c
    Nothing -> throw (ClassNotFound name)

-- | Get class field signature from current ClassPath
getClassField :: (Throws ENotFound, Throws ENotLoaded, MonadThrow m, MonadState GState m, MonadIO m)
              => String -> B.ByteString -> m (NameType (Field Direct))
getClassField clsName fldName = do
  cls <- getClass clsName
  case lookupField fldName cls of
    Just fld -> return (fieldNameType fld)
    Nothing  -> throw (FieldNotFound clsName fldName)

-- | Get class method signature from current ClassPath
getClassMethod :: (Throws ENotFound, Throws ENotLoaded, MonadThrow m, MonadState GState m, MonadIO m)
               => String -> B.ByteString -> m (NameType (Method Direct))
getClassMethod clsName mName = do
  cls <- getClass clsName
  case lookupMethod mName cls of
    Just m -> return (methodNameType m)
    Nothing  -> throw (MethodNotFound clsName mName)

-- | Access the generated bytecode length
encodedCodeLength :: GState -> Word32
encodedCodeLength st = fromIntegral . B.length . encodeInstructions $ generated st

-- generateCodeLength :: Generate (Caught SomeException NoExceptions) a -> Word32
generateCodeLength = encodedCodeLength . execGenerate []

-- | Convert Generator state to method Code.
genCode :: GState -> Code
genCode st = Code {
    codeStackSize = stackSize st,
    codeMaxLocals = locals st,
    codeLength = encodedCodeLength st,
    codeInstructions = generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = AP [] }

-- | Start class generation.
initClass :: (Monad m, MonadState GState m) => B.ByteString -> m Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

-- | Generate a class
generateIO ::Monad m
              => [Tree CPEntry]
              -> B.ByteString
              -> Generate e m a
              -> m (Class Direct)
generateIO cp name gen = do
  let generator = do
        initClass name
        gen
  res <- execGenerateIO cp generator
  let code = genCode res
      d = defaultClass :: Class Direct
  return $ d {
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res }

-- | Generate a class
generate :: [Tree CPEntry]
            -> B.ByteString
            -> Generate e Identity a
            -> Class Direct
generate cp name gen =
  runIdentity $ generateIO cp name gen