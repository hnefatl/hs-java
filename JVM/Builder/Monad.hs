{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | This module defines Generate[IO] monad, which helps generating JVM code and
-- creating Java class constants pool.
--
-- Code generation could be done using one of two monads: Generate and GenerateIO.
-- Generate monad is pure (simply State monad), while GenerateIO is IO-related.
-- In GenerateIO additional actions are available, such as setting up ClassPath
-- and loading classes (from .class files or JAR archives).
--
module JVM.Builder.Monad (
    GState(..),
    emptyGState,
    MonadGenerator(..),
    GeneratorT(..),
    Generator, GeneratorIO,
    execGeneratorT,
    evalGeneratorT,
    runGeneratorT,
    addToPool,
    addSig,
    i0, i1, i8,
    newMethod,
    newField,
    setStackSize, setMaxLocals,
    withClassPath,
    getClassField, getClassMethod,
    generate, generateT, generateIO,
    encodedCodeLength,
    generateCodeLength,
    getMethodLength,
    genCode
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State        as St
import qualified Control.Monad.State.Strict as StS
import           Data.Binary                hiding (get, put)
import qualified Data.ByteString.Lazy       as B
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Prelude

import           Java.ClassPath
import           JVM.Assembler
import           JVM.ClassFile
import           JVM.Common
import           JVM.Exceptions

-- | Generator state
data GState = GState {
  generated      :: [[Instruction]],             -- ^ Stack of already generated code (head is for current method)
  currentPool    :: Pool Direct,             -- ^ Already generated constants pool
  nextPoolIndex  :: Word16,                -- ^ Next index to be used in constants pool
  doneMethods    :: [Method Direct],         -- ^ Already generated class methods
  currentMethods :: [Method Direct], -- ^ Stack of methods being created - head is current.
  fields         :: [Field Direct],
  stackSize      :: Word16,                    -- ^ Maximum stack size for current method
  locals         :: Word16,                       -- ^ Maximum number of local variables for current method
  classPath      :: [Tree CPEntry]
  }
  deriving (Eq,Show)

-- | Empty generator state
emptyGState ::  GState
emptyGState = GState {
  generated = [],
  currentPool = M.empty,
  nextPoolIndex = 1,
  doneMethods = [],
  fields = [],
  currentMethods = [],
  stackSize = 496,
  locals = 0,
  classPath = []}

class Monad m => MonadGenerator m where
    getGState :: m GState
    putGState :: GState -> m ()
    throwG :: GeneratorException -> m a

instance MonadGenerator m => MonadGenerator (ExceptT e m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG
instance MonadGenerator m => MonadGenerator (ReaderT e m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG
instance MonadGenerator m => MonadGenerator (StateT e m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG
instance MonadGenerator m => MonadGenerator (StS.StateT e m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG
instance MonadGenerator m => MonadGenerator (IdentityT m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG

modifyGState :: MonadGenerator m => (GState -> GState) -> m ()
modifyGState fn = do
  st <- getGState
  putGState $ fn st
getsGState :: MonadGenerator m => (GState -> a) -> m a
getsGState = (<$> getGState)


-- | Generate monad
newtype GeneratorT m a = GeneratorT (ExceptT GeneratorException (StateT GState m) a)
  deriving (Functor, Applicative, Monad)
instance MonadTrans GeneratorT where
    lift = GeneratorT . lift . lift
instance Monad m => MonadGenerator (GeneratorT m) where
    getGState = GeneratorT get
    putGState = GeneratorT . put
    throwG = GeneratorT . throwError

type GeneratorIO = GeneratorT IO
type Generator = GeneratorT Identity

execGeneratorT :: Monad m => [Tree CPEntry] -> GeneratorT m a -> ExceptT GeneratorException m GState
execGeneratorT cp = fmap snd . runGeneratorT cp
evalGeneratorT :: Monad m => [Tree CPEntry] -> GeneratorT m a -> ExceptT GeneratorException m a
evalGeneratorT cp = fmap fst . runGeneratorT cp
runGeneratorT :: Monad m => [Tree CPEntry] -> GeneratorT m a -> ExceptT GeneratorException m (a, GState)
runGeneratorT cp (GeneratorT inner) = do
    (x, s) <- lift $ runStateT (runExceptT inner) (emptyGState { classPath = cp })
    case x of
        Left err -> throwError err
        Right y  -> return (y, s)

instance MonadError GeneratorException m => MonadError GeneratorException (GeneratorT m) where
    throwError = lift . throwError
    catchError (GeneratorT x) f = GeneratorT $ catchError x $ (\(GeneratorT y) -> y) . f
instance MonadState s m => MonadState s (GeneratorT m) where
    state = lift . state
instance MonadReader s m => MonadReader s (GeneratorT m) where
    ask = lift ask
    local f (GeneratorT x) = GeneratorT $ local f x
instance MonadIO m => MonadIO (GeneratorT m) where
    liftIO = lift . liftIO


-- | Update ClassPath
withClassPath :: (MonadIO m, MonadGenerator m) => ClassPath () -> m ()
withClassPath cp = do
  res <- liftIO $ execClassPath cp
  st <- getGState
  putGState $ st {classPath = res}

-- | Add a constant to pool
addItem :: MonadGenerator m => Constant Direct -> m Word16
addItem c = do
    pool <- getsGState currentPool
    case lookupPool c pool of
        Just i -> return i
        Nothing -> do
            i <- getsGState nextPoolIndex
            let pool' = M.insert i c pool
                i' = if long c then i+2 else i+1
            modifyGState $ \st -> st { currentPool = pool', nextPoolIndex = i' }
            return i

-- | Lookup in a pool
lookupPool :: Constant Direct -> Pool Direct -> Maybe Word16
lookupPool c pool =
  fromIntegral `fmap` mapFindIndex (== c) pool

addNT :: (MonadGenerator m, HasSignature a) => NameType a -> m Word16
addNT (NameType name sig) = do
  let bsig = encode sig
  x <- addItem (CNameType name bsig)
  _ <- addItem (CUTF8 name)
  _ <- addItem (CUTF8 bsig)
  return x

addSig :: MonadGenerator m => MethodSignature -> m Word16
addSig c@MethodSignature{} = addItem (CUTF8 bsig)
    where bsig = encode c

-- | Add a constant into pool
addToPool :: MonadGenerator m => Constant Direct -> m Word16
addToPool c@(CClass str) = do
  _ <- addItem (CUTF8 str)
  addItem c
addToPool c@(CField cls name) = do
  _ <- addToPool (CClass cls)
  _ <- addNT name
  addItem c
addToPool c@(CMethod cls name) = do
  _ <- addToPool (CClass cls)
  _ <- addNT name
  addItem c
addToPool c@(CIfaceMethod cls name) = do
  _ <- addToPool (CClass cls)
  _ <- addNT name
  addItem c
addToPool c@(CString str) = do
  _ <- addToPool (CUTF8 str)
  addItem c
addToPool c@(CNameType name sig) = do
  _ <- addItem (CUTF8 name)
  _ <- addItem (CUTF8 sig)
  addItem c
addToPool c@(CMethodHandle _ cls method) = do
    _ <- addToPool (CMethod cls (methodNameType method))
    addItem c
addToPool c@(CMethodType sig) = do
    _ <- addItem (CUTF8 sig)
    addItem c
addToPool c@(CInvokeDynamic _ nt) = addNT nt >> addItem c
addToPool c = addItem c

putInstruction :: MonadGenerator m => Instruction -> m ()
putInstruction instr = do
    st <- getGState
    case generated st of
        []                    -> throwG NoMethodUnderConstruction
        currentGen:stackedGen -> putGState $ st { generated = (currentGen ++ [instr]):stackedGen }

-- | Generate one (zero-arguments) instruction
i0 :: MonadGenerator m => Instruction -> m ()
i0 = putInstruction

-- | Generate one one-argument instruction
i1 :: MonadGenerator m => (Word16 -> Instruction) -> Constant Direct -> m ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

-- | Generate one one-argument instruction
i8 :: MonadGenerator m => (Word8 -> Instruction) -> Constant Direct -> m ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

-- | Set maximum stack size for current method
setStackSize :: MonadGenerator m => Word16 -> m ()
setStackSize n = modifyGState $ \st -> st {stackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: MonadGenerator m => Word16 -> m ()
setMaxLocals n = modifyGState $ \st -> st {locals = n}

-- | Start generating new method
startMethod :: MonadGenerator m => [AccessFlag] -> B.ByteString -> MethodSignature -> m ()
startMethod flags name sig = do
  _ <- addToPool (CString name)
  _ <- addSig sig
  setStackSize 4096
  setMaxLocals 100
  st <- getGState
  let method = Method {
    methodAccessFlags = S.fromList flags,
    methodName = name,
    methodSignature = sig,
    methodAttributesCount = 0,
    methodAttributes = AR M.empty }
  putGState $ st {generated = []:generated st,
               currentMethods = method:currentMethods st }

-- | End of method generation
endMethod :: MonadGenerator m => m ()
endMethod = do
    m <- getsGState currentMethods
    code <- getsGState genCode
    case m of
        [] -> throwG UnexpectedEndMethod
        method:stackedMethods -> do
            let method' = method {methodAttributes = AR $ M.fromList [("Code", encodeMethod code)],
                                  methodAttributesCount = 1}
            modifyGState $ \st ->
                     st {generated = tail $ generated st,
                         currentMethods = stackedMethods,
                         doneMethods = doneMethods st ++ [method']}

-- | Generate new method
newMethod :: MonadGenerator m
          => [AccessFlag]        -- ^ Access flags for method (public, static etc)
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

newField :: MonadGenerator m => [AccessFlag] -> B.ByteString -> FieldType -> m (NameType (Field Direct))
newField access name desc = do
    void $ addToPool (CString name)
    void $ addToPool (CUTF8 $ encode desc)
    s <- getGState
    let field = Field
            { fieldAccessFlags = S.fromList access
            , fieldName = name
            , fieldSignature = desc
            , fieldAttributesCount = 0
            , fieldAttributes = AR M.empty }
    putGState s { fields = field:fields s }
    return $ fieldNameType field

-- | Get a class from current ClassPath
getClass :: (MonadIO m, MonadGenerator m) => String -> m (Class Direct)
getClass name = do
    cp <- getsGState classPath
    res <- liftIO $ getEntry cp name
    case res of
        Just (NotLoaded p)      -> throwG (ClassFileNotLoaded p)
        Just (Loaded _ c)       -> return c
        Just (NotLoadedJAR p c) -> throwG (JARNotLoaded p c)
        Just (LoadedJAR _ c)    -> return c
        Nothing                 -> throwG (ClassNotFound name)

-- | Get class field signature from current ClassPath
getClassField :: (MonadIO m, MonadGenerator m) => String -> B.ByteString -> m (NameType (Field Direct))
getClassField clsName fldName = do
    cls <- getClass clsName
    case lookupField fldName cls of
        Just fld -> return (fieldNameType fld)
        Nothing  -> throwG (FieldNotFound clsName fldName)

-- | Get class method signature from current ClassPath
getClassMethod :: (MonadIO m, MonadGenerator m) => String -> B.ByteString -> m (NameType (Method Direct))
getClassMethod clsName mName = do
    cls <- getClass clsName
    case lookupMethod mName cls of
        Just m  -> return (methodNameType m)
        Nothing -> throwG (MethodNotFound clsName mName)

-- | Access the generated bytecode length
encodedCodeLength :: GState -> Word32
encodedCodeLength st = fromIntegral . B.length . encodeInstructions $ head $ generated st

generateCodeLength :: Monad m => GeneratorT m a -> ExceptT GeneratorException m Word32
generateCodeLength x = encodedCodeLength <$> execGeneratorT [] x

getMethodLength :: MonadGenerator m => m Word32
getMethodLength = encodedCodeLength <$> getGState

-- | Convert Generator state to method Code.
genCode :: GState -> Code
genCode st = Code {
    codeStackSize = stackSize st,
    codeMaxLocals = locals st,
    codeLength = encodedCodeLength st,
    codeInstructions = head $ generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = AP [] }

-- | Start class generation.
initClass :: MonadGenerator m => B.ByteString -> m Word16
initClass name = do
    _ <- addToPool (CClass "java/lang/Object")
    _ <- addToPool (CClass name)
    addToPool (CString "Code")

-- | Generate a class
generateT :: Monad m
           => [Tree CPEntry]
           -> B.ByteString
           -> GeneratorT m a
           -> ExceptT GeneratorException m (a, Class Direct)
generateT cp name gen = do
    let generator = initClass name >> gen
        d = defaultClass :: Class Direct
    (x, res) <- runGeneratorT cp generator
    return $ (x,) $ d {
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res,
        classFields = fields res }

generateIO :: [Tree CPEntry]
         -> B.ByteString
         -> GeneratorIO a
         -> IO (a, Class Direct)
generateIO cp name gen = do
    x <- runExceptT $ generateT cp name gen
    case x of
        Left err  -> error (show err)
        Right res -> return res

-- | Generate a class
generate :: [Tree CPEntry]
         -> B.ByteString
         -> Generator a
         -> Except GeneratorException (a, Class Direct)
generate = generateT
