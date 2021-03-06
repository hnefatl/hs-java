{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
-- | This module declares (low-level) data types for Java .class files
-- structures, and Binary instances to read/write them.
module JVM.ClassFile
  (-- * About
   -- $about
   --
   --
   -- * Internal class file structures
   Attribute (..),
   FieldType (..),
   -- * Signatures
   FieldSignature, MethodSignature (..), ReturnSignature (..),
   ArgumentSignature,
   -- * Stage types
   File, Direct,
   -- * Staged structures
   Pool, Link,
   Method (..), Field (..), Class (..),
   Constant (..),
   AccessFlag (..), AccessFlags,
   Attributes (..),
   defaultClass,
   -- * Misc
   HasSignature (..), HasAttributes (..),
   NameType (..),
   MethodHandleKind(..),
   BootstrapMethodsAttribute(..),
   BootstrapMethod(..),
   AttributeConvertible(..),
   mhtToWord, wordToMht,
   fieldNameType, methodNameType,
   lookupField, lookupMethod,
   long,
   toString,
   className,
   apsize, arsize, arlist
  )
  where

import           Codec.Binary.UTF8.String hiding (decode, encode)
import           Control.Monad
import           Control.Monad.Except     (Except, liftEither, runExcept)
import qualified Control.Monad.State      as St
import           Control.Monad.Trans      (lift)
import           Control.DeepSeq          (NFData)
import           Data.Bifunctor           (bimap, second)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Binary.Put
import qualified Data.BinaryState         as BinaryState
import qualified Data.Bimap               as Bimap
import qualified Data.ByteString.Lazy     as B
import           Data.Char
import           Data.Default
import           Data.List
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           GHC.Generics             (Generic)
import           JVM.Common               ()

-- $about
--
-- Java .class file uses constants pool, which stores almost all source-code-level
-- constants (strings, integer literals etc), and also all identifiers (class,
-- method, field names etc). All other structures contain indexes of constants in
-- the pool instead of constants theirself.
--
-- It's not convient to use that indexes programmatically. So, .class file is represented
-- at two stages: File and Direct. At File stage, all data structures contain only indexes,
-- not constants theirself. When we read a class from a file, we get structure at File stage.
-- We only can write File stage structure to file.
--
-- At Direct stage, structures conain constants, not indexes. Convertion functions (File <-> Direct)
-- are located in the JVM.Converter module.
--

-- | Read one-byte Char
getChar8 :: Get Char
getChar8 = chr . fromIntegral <$> getWord8

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

-- | File stage
data File

-- | Direct representation stage
data Direct

-- | Link to some object
type family Link stage a

-- | At File stage, Link contain index of object in the constants pool.
type instance Link File a = Word16

-- | At Direct stage, Link contain object itself.
type instance Link Direct a = a

-- | Object (class, method, field …) access flags
type family AccessFlags stage

-- | At File stage, access flags are represented as Word16
type instance AccessFlags File = Word16

-- | At Direct stage, access flags are represented as set of flags.
type instance AccessFlags Direct = S.Set AccessFlag

-- | Object (class, method, field) attributes
data family Attributes stage

-- | At File stage, attributes are represented as list of Attribute structures.
data instance Attributes File = AP {attributesList :: [Attribute]}
  deriving (Eq, Ord, Show)

instance Default (Attributes File) where
  def = AP []
deriving instance Generic (Attributes File)
instance NFData (Attributes File)

-- | At Direct stage, attributes are represented as a Map.
data instance Attributes Direct = AR (M.Map B.ByteString B.ByteString)
  deriving (Eq, Ord, Show)

instance Default (Attributes Direct) where
  def = AR M.empty

-- | Size of attributes set at Direct stage
arsize :: Attributes Direct -> Int
arsize (AR m) = M.size m

-- | Associative list of attributes at Direct stage
arlist :: Attributes Direct -> [(B.ByteString, B.ByteString)]
arlist (AR m) = M.assocs m

-- | Size of attributes set at File stage
apsize :: Attributes File -> Int
apsize (AP list) = length list

-- | Access flags. Used for classess, methods, variables.
data AccessFlag =
    ACC_PUBLIC          -- ^ 0x0001 Visible for all
  | ACC_PRIVATE        -- ^ 0x0002 Visible only for defined class
  | ACC_PROTECTED      -- ^ 0x0004 Visible only for subclasses
  | ACC_STATIC          -- ^ 0x0008 Static method or variable
  | ACC_FINAL          -- ^ 0x0010 No further subclassing or assignments
  | ACC_SYNCHRONIZED -- ^ 0x0020 Uses monitors
  | ACC_VOLATILE        -- ^ 0x0040 Could not be cached
  | ACC_TRANSIENT      -- ^ 0x0080
  | ACC_NATIVE          -- ^ 0x0100 Implemented in other language
  | ACC_INTERFACE      -- ^ 0x0200 Class is interface
  | ACC_ABSTRACT        -- ^ 0x0400
  deriving (Eq, Show, Ord, Enum)

-- | Fields and methods have signatures.
class (Binary (Signature a), Show (Signature a), Eq (Signature a), Ord (Signature a)) => HasSignature a where
  type Signature a

instance HasSignature (Field Direct) where
  type Signature (Field Direct) = FieldSignature

instance HasSignature (Method Direct) where
  type Signature (Method Direct) = MethodSignature

-- | Name and signature pair. Used for methods and fields.
data NameType a = NameType {
  ntName      :: B.ByteString,
  ntSignature :: Signature a }

instance (HasSignature a) => Show (NameType a) where
  show (NameType n t) = toString n ++ ": " ++ show t

deriving instance (HasSignature a, Eq a) => Eq (NameType a)
deriving instance (HasSignature a, Ord a) => Ord (NameType a)

instance HasSignature a => Binary (NameType a) where
  put (NameType n t) = putInt64be (fromIntegral $ B.length n) >> putLazyByteString n >> put t

  get = do
    n <- getInt64be
    NameType <$> getLazyByteString n <*> get

-- | Constant pool item
-- Follows https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data Constant stage =
    CClass (Link stage B.ByteString)
  | CField (Link stage B.ByteString) (Link stage (NameType (Field stage)))
  | CMethod (Link stage B.ByteString) (Link stage (NameType (Method stage)))
  | CIfaceMethod (Link stage B.ByteString) (Link stage (NameType (Method stage)))
  | CString (Link stage B.ByteString)
  | CInteger Word32
  | CFloat Float
  | CLong Word64
  | CDouble Double
  | CNameType (Link stage B.ByteString) (Link stage B.ByteString)
  | CUTF8 {getString :: B.ByteString}
  | CUnicode {getString :: B.ByteString}
  | CMethodHandle MethodHandleKind (Link stage B.ByteString) (Link stage (Method stage)) -- handle type, class, method
  | CMethodType (Link stage B.ByteString)
  | CInvokeDynamic Word16 (Link stage (NameType (Method stage)))
deriving instance Ord (Constant Direct)
deriving instance Ord (Constant File)

-- https://docs.oracle.com/javase/8/docs/api/java/lang/invoke/MethodHandleInfo.html
data MethodHandleKind =
      GetField
    | GetStatic
    | PutField
    | PutStatic
    | InvokeVirtual
    | InvokeStatic
    | InvokeSpecial
    | NewInvokeSpecial
    | InvokeInterface
    deriving (Eq, Ord, Generic)
instance NFData MethodHandleKind
mhtToWord :: MethodHandleKind -> Word8
mhtToWord GetField         = 1
mhtToWord GetStatic        = 2
mhtToWord PutField         = 3
mhtToWord PutStatic        = 4
mhtToWord InvokeVirtual    = 5
mhtToWord InvokeStatic     = 6
mhtToWord InvokeSpecial    = 7
mhtToWord NewInvokeSpecial = 8
mhtToWord InvokeInterface  = 9
wordToMht :: Word8 -> MethodHandleKind
wordToMht 1 = GetField
wordToMht 2 = GetStatic
wordToMht 3 = PutField
wordToMht 4 = PutStatic
wordToMht 5 = InvokeVirtual
wordToMht 6 = InvokeStatic
wordToMht 7 = InvokeSpecial
wordToMht 8 = NewInvokeSpecial
wordToMht 9 = InvokeInterface
wordToMht i = error $ "Invalid word for MethodHandleKind: " <> show i

instance Show MethodHandleKind where
    show = show . mhtToWord
instance Binary MethodHandleKind where
    put = put . mhtToWord
    get = wordToMht <$> get

-- | Name of the CClass. Error on any other constant.
className ::  Constant Direct -> B.ByteString
className (CClass s) = s
className x          = error $ "Not a class: " ++ show x

instance Show (Constant Direct) where
  show (CClass name)           = "Class " ++ toString name
  show (CField cls nt)         = "Fieldref " ++ toString cls ++ "." ++ show nt
  show (CMethod cls nt)        = "Methodref " ++ toString cls ++ "." ++ show nt
  show (CIfaceMethod cls nt)   = "InterfaceMethodref " ++ toString cls ++ "." ++ show nt
  show (CString s)             = "String \"" ++ toString s ++ "\""
  show (CInteger x)            = "Integer" ++ show x
  show (CFloat x)              = "Float " ++ show x
  show (CLong x)               = "Long " ++ show x
  show (CDouble x)             = "Double " ++ show x
  show (CNameType name tp)     = "NameAndType " ++ toString name ++ ":" ++ toString tp
  show (CUTF8 s)               = "UTF8 \"" ++ toString s ++ "\""
  show (CUnicode s)            = "Unicode \"" ++ toString s ++ "\""
  show (CMethodHandle t cls b) = "MethodHandle " ++ show t ++ " " ++ show b ++ " (class " ++ show cls ++ ")"
  show (CMethodType b)         = "MethodType " ++ toString b
  show (CInvokeDynamic t m)    = "InvokeDynamic " ++ show t ++ ":" ++ show m

-- | Constant pool
type Pool stage = Bimap.Bimap Word16 (Constant stage)

-- | Generic .class file format
data Class stage = Class {
  magic                :: Word32,                         -- ^ Magic value: 0xCAFEBABE
  minorVersion         :: Word16,
  majorVersion         :: Word16,
  constsPoolSize       :: Word16,                -- ^ Number of items in constants pool
  constsPool           :: Pool stage,                -- ^ Constants pool itself
  accessFlags          :: AccessFlags stage,        -- ^ See @JVM.Types.AccessFlag@
  thisClass            :: Link stage B.ByteString,    -- ^ Constants pool item index for this class
  superClass           :: Link stage B.ByteString,   -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount      :: Word16,               -- ^ Number of implemented interfaces
  interfaces           :: [Link stage B.ByteString], -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount     :: Word16,              -- ^ Number of class fileds
  classFields          :: [Field stage],            -- ^ Class fields
  classMethodsCount    :: Word16,             -- ^ Number of class methods
  classMethods         :: [Method stage],          -- ^ Class methods
  classAttributesCount :: Word16,          -- ^ Number of class attributes
  classAttributes      :: Attributes stage      -- ^ Class attributes
  }

deriving instance Eq (Class File)
deriving instance Eq (Class Direct)
deriving instance Show (Class File)
deriving instance Show (Class Direct)

deriving instance Generic (Class File)
instance NFData (Class File)

deriving instance Eq (Constant File)
deriving instance Eq (Constant Direct)
deriving instance Show (Constant File)

deriving instance Generic (Constant File)
instance NFData (Constant File)

-- | Default (empty) class file definition.
defaultClass :: (Default (AccessFlags stage), Default (Link stage B.ByteString), Default (Attributes stage))
             => Class stage
defaultClass = Class {
  magic = 0xCAFEBABE,
  minorVersion = 0,
  majorVersion = 52,
  constsPoolSize = 0,
  constsPool = Bimap.empty,
  accessFlags = def,
  thisClass = def,
  superClass = def,
  interfacesCount = 0,
  interfaces = [],
  classFieldsCount = 0,
  classFields = [],
  classMethodsCount = 0,
  classMethods = [],
  classAttributesCount = 0,
  classAttributes = def }

instance Binary (Class File) where
  put Class {..} = do
    put magic
    put minorVersion
    put majorVersion
    putPool constsPool
    put accessFlags
    put thisClass
    put superClass
    put interfacesCount
    forM_ interfaces put
    put classFieldsCount
    forM_ classFields put
    put classMethodsCount
    forM_ classMethods put
    put classAttributesCount
    forM_ (attributesList classAttributes) put

  get = do
    magic <- get
    when (magic /= 0xCAFEBABE) $
      fail $ "Invalid .class file MAGIC value: " ++ show magic
    minor <- get
    major <- get
    when (major > 52) $
      fail $ "Too new .class file format: " ++ show major
    poolsize <- getWord16be
    pool <- getPool (poolsize - 1)
    af <-  get
    this <- get
    super <- get
    interfacesCount <- get
    ifaces <- replicateM (fromIntegral interfacesCount) get
    classFieldsCount <- getWord16be
    classFields <- replicateM (fromIntegral classFieldsCount) get
    classMethodsCount <- get
    classMethods <- replicateM (fromIntegral classMethodsCount) get
    asCount <- get
    as <- replicateM (fromIntegral asCount) get
    return $ Class magic minor major poolsize pool af this super
               interfacesCount ifaces classFieldsCount classFields
               classMethodsCount classMethods asCount (AP as)

-- | Field signature format
data FieldType =
    SignedByte -- ^ B
  | CharByte   -- ^ C
  | DoubleType -- ^ D
  | FloatType  -- ^ F
  | IntType    -- ^ I
  | LongInt    -- ^ J
  | ShortInt   -- ^ S
  | BoolType   -- ^ Z
  | ObjectType String -- ^ L @{class name}@
  | Array (Maybe Int) FieldType -- ^ @[{type}@
  deriving (Eq, Ord)

instance Show FieldType where
  show SignedByte         = "byte"
  show CharByte           = "char"
  show DoubleType         = "double"
  show FloatType          = "float"
  show IntType            = "int"
  show LongInt            = "long"
  show ShortInt           = "short"
  show BoolType           = "bool"
  show (ObjectType s)     = "Object " ++ s
  show (Array Nothing t)  = show t ++ "[]"
  show (Array (Just n) t) = show t ++ "[" ++ show n ++ "]"

-- | Class field signature
type FieldSignature = FieldType

-- | Try to read integer value from decimal representation
getInt :: Get (Maybe Int)
getInt = do
    s <- getDigits
    if null s
      then return Nothing
      else return $ Just (read s)
  where
    getDigits :: Get String
    getDigits = do
      c <- lookAhead getChar8
      if isDigit c
        then do
             skip 1
             next <- getDigits
             return (c: next)
        else return []

putString :: String -> Put
putString str = forM_ str put

instance Binary FieldType where
  put SignedByte           = put 'B'
  put CharByte             = put 'C'
  put DoubleType           = put 'D'
  put FloatType            = put 'F'
  put IntType              = put 'I'
  put LongInt              = put 'J'
  put ShortInt             = put 'S'
  put BoolType             = put 'Z'
  put (ObjectType name)    = put 'L' >> putString name >> put ';'
  put (Array Nothing sig)  = put '[' >> put sig
  put (Array (Just n) sig) = put '[' >> put (show n) >> put sig

  get = do
    b <- getChar8
    case b of
      'B' -> return SignedByte
      'C' -> return CharByte
      'D' -> return DoubleType
      'F' -> return FloatType
      'I' -> return IntType
      'J' -> return LongInt
      'S' -> return ShortInt
      'Z' -> return BoolType
      'L' -> ObjectType <$> getToSemicolon
      '[' -> Array <$> getInt <*> get
      _   -> fail $ "Unknown signature opening symbol: " ++ [b]

-- | Read string up to `;'
getToSemicolon :: Get String
getToSemicolon = do
    x <- get
    if x == ';' then return [] else do
        next <- getToSemicolon
        return (x: next)

-- | Return value signature
data ReturnSignature =
    Returns FieldType
  | ReturnsVoid
  deriving (Eq, Ord)

instance Show ReturnSignature where
  show (Returns t) = show t
  show ReturnsVoid = "Void"

instance Binary ReturnSignature where
  put (Returns sig) = put sig
  put ReturnsVoid   = put 'V'

  get = do
    x <- lookAhead getChar8
    case x of
      'V' -> skip 1 >> return ReturnsVoid
      _   -> Returns <$> get

-- | Method argument signature
type ArgumentSignature = FieldType

-- | Class method argument signature
data MethodSignature = MethodSignature [ArgumentSignature] ReturnSignature
    deriving (Eq, Ord)

instance Show MethodSignature where
    show (MethodSignature args ret) = "(" ++ intercalate ", " (map show args) ++ ") returns " ++ show ret

instance Binary MethodSignature where
    put (MethodSignature args ret) = do
        put '('
        forM_ args put
        put ')'
        put ret

    get =  do
        x <- getChar8
        when (x /= '(') $ fail "Cannot parse method signature: no starting `(' !"
        args <- getArgs
        y <- getChar8
        when (y /= ')') $ fail "Internal error: method signature without `)' !?"
        MethodSignature args <$> get

-- | Read arguments signatures (up to `)')
getArgs :: Get [ArgumentSignature]
getArgs = whileJust getArg
  where
    getArg :: Get (Maybe ArgumentSignature)
    getArg = do
      x <- lookAhead getChar8
      if x == ')' then return Nothing else Just <$> get

whileJust :: (Monad m) => m (Maybe a) -> m [a]
whileJust m = do
  r <- m
  case r of
    Just x  -> (x:) <$> whileJust m
    Nothing -> return []

long :: Constant stage -> Bool
long (CLong _)   = True
long (CDouble _) = True
long _           = False

putPool :: Pool File -> Put
putPool pool = do
    -- Don't use elems, as we want the items sorted by the LHS values (the pool indices, which need to be ascending)
    let list = map snd $ Bimap.toAscList pool
        d = length $ filter long list
    putWord16be $ fromIntegral (Bimap.size pool + d + 1)
    forM_ list putC
  where
    putC (CClass i)            = putWord8 7 >> put i
    putC (CField i j)          = putWord8 9 >> put i >> put j
    putC (CMethod i j)         = putWord8 10 >> put i >> put j
    putC (CIfaceMethod i j)    = putWord8 11 >> put i >> put j
    putC (CString i)           = putWord8 8 >> put i
    putC (CInteger x)          = putWord8 3 >> put x
    putC (CFloat x)            = putWord8 4 >> putFloat32be x
    putC (CLong x)             = putWord8 5 >> put x
    putC (CDouble x)           = putWord8 6 >> putFloat64be x
    putC (CNameType i j)       = putWord8 12 >> put i >> put j
    putC (CUTF8 bs)            = putWord8 1 >> put (fromIntegral (B.length bs) :: Word16) >> putLazyByteString bs
    putC (CUnicode bs)         = putWord8 2 >> put (fromIntegral (B.length bs) :: Word16) >> putLazyByteString bs
    putC (CMethodHandle t _ b) = putWord8 15 >> put t >> put b
    putC (CMethodType b)       = putWord8 16 >> put b
    putC (CInvokeDynamic t m)  = putWord8 18 >> put t >> put m

getPool :: Word16 -> Get (Pool File)
getPool n = do
    items <- St.evalStateT go 1
    -- Add the class names to the CMethodHandle constants
    let addClasses (CMethodHandle t _ b) = case fmap snd $ find ((== b) . fst) items of
            Just (CClass name) -> CMethodHandle t name b
            Just _             -> error "Found non-class for MethodHandle"
            Nothing            -> error "No class found for MethodHandle"
        addClasses x = x
        items' = Bimap.fromList $ map (second addClasses) items
    return items'
  where
    go :: St.StateT Word16 Get [(Word16, Constant File)]
    go = do
      i <- St.get
      if i > n
        then return []
        else do
          c <- lift getC
          let i' = if long c then i+2 else i+1
          St.put i'
          next <- go
          return $ (i,c): next

    getC = do
      tag <- getWord8
      case tag of
        1 -> do
          l <- get
          bs <- getLazyByteString (fromIntegral (l :: Word16))
          return $ CUTF8 bs
        2 -> do
          l <- get
          bs <- getLazyByteString (fromIntegral (l :: Word16))
          return $ CUnicode bs
        3  -> CInteger   <$> get
        4  -> CFloat     <$> getFloat32be
        5  -> CLong      <$> get
        6  -> CDouble    <$> getFloat64be
        7  -> CClass     <$> get
        8  -> CString    <$> get
        9  -> CField     <$> get <*> get
        10 -> CMethod    <$> get <*> get
        11 -> CIfaceMethod <$> get <*> get
        12 -> CNameType    <$> get <*> get
        15 -> CMethodHandle <$> get <*> pure 0 <*> get
        16 -> CMethodType <$> get
        18 -> CInvokeDynamic <$> get <*> get
        _  -> fail $ "Unknown constants pool entry tag: " ++ show tag

-- | Class field format
data Field stage = Field {
  fieldAccessFlags     :: AccessFlags stage,
  fieldName            :: Link stage B.ByteString,
  fieldSignature       :: Link stage FieldSignature,
  fieldAttributesCount :: Word16,
  fieldAttributes      :: Attributes stage }

deriving instance Eq (Field File)
deriving instance Eq (Field Direct)
deriving instance Ord (Field File)
deriving instance Ord (Field Direct)
deriving instance Show (Field File)
deriving instance Show (Field Direct)
deriving instance Generic (Field File)
instance NFData (Field File)

lookupField :: B.ByteString -> Class Direct -> Maybe (Field Direct)
lookupField name cls = look (classFields cls)
  where
    look [] = Nothing
    look (f:fs)
      | fieldName f == name = Just f
      | otherwise           = look fs

fieldNameType :: Field Direct -> NameType (Field Direct)
fieldNameType f = NameType (fieldName f) (fieldSignature f)

instance Binary (Field File) where
  put Field {..} = do
    put fieldAccessFlags
    put fieldName
    put fieldSignature
    put fieldAttributesCount
    forM_ (attributesList fieldAttributes) put

  get = do
    af <- get
    ni <- getWord16be
    si <- get
    n <- getWord16be
    as <- replicateM (fromIntegral n) get
    return $ Field af ni si n (AP as)

-- | Class method format
data Method stage = Method {
  methodAccessFlags     :: AccessFlags stage,
  methodName            :: Link stage B.ByteString,
  methodSignature       :: Link stage MethodSignature,
  methodAttributesCount :: Word16,
  methodAttributes      :: Attributes stage }

deriving instance Eq (Method File)
deriving instance Eq (Method Direct)
deriving instance Ord (Method File)
deriving instance Ord (Method Direct)
deriving instance Show (Method File)
deriving instance Show (Method Direct)
deriving instance Generic (Method File)
instance NFData (Method File)

methodNameType :: Method Direct -> NameType (Method Direct)
methodNameType m = NameType (methodName m) (methodSignature m)

lookupMethod :: B.ByteString -> Class Direct -> Maybe (Method Direct)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise           = look fs

instance Binary (Method File) where
  put Method {..} = do
    put methodAccessFlags
    put methodName
    put methodSignature
    put methodAttributesCount
    forM_ (attributesList methodAttributes) put

  get = do
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return $ Method {
               methodAccessFlags = af,
               methodName = ni,
               methodSignature = si,
               methodAttributesCount = n,
               methodAttributes = AP as }

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data Attribute = Attribute {
  attributeName   :: Word16,
  attributeLength :: Word32,
  attributeValue  :: B.ByteString }
  deriving (Eq, Ord, Show, Generic)
instance NFData Attribute

instance BinaryState.BinaryState Integer Attribute where
  put a = do
    let sz = 6 + attributeLength a      -- full size of AttributeInfo structure
    BinaryState.liftOffset (fromIntegral sz) put a

  get = BinaryState.getZ

instance Binary Attribute where
  put Attribute {..} = do
    put attributeName
    putWord32be attributeLength
    putLazyByteString attributeValue

  get = do
    name <- getWord16be
    len <- getWord32be
    value <- getLazyByteString (fromIntegral len)
    return $ Attribute name len value

class AttributeConvertible a where
    toAttribute :: a -> Attribute
    fromAttribute :: Attribute -> Either String a

decodeOrFail' :: Binary a => B.ByteString -> Either String (a, B.ByteString)
decodeOrFail' = bimap (\(_,_,x) -> x) (\(x,_,y) -> (y,x)) . decodeOrFail

-- |The BootstrapMethods attribute
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.23
data BootstrapMethodsAttribute = BootstrapMethodsAttribute
    { attributeNameIndex :: Word16 -- An index into the constant pool to a UTF8 entry containing "BootstrapMethods"
    , attributeMethods   :: [BootstrapMethod] }
    deriving (Eq, Ord, Show)
data BootstrapMethod = BootstrapMethod
    { bootstrapMethodRef :: Word16
    , bootstrapArguments :: [Word16] }
    deriving (Eq, Ord, Show)
instance Binary BootstrapMethod where
    put m = do
        put $ bootstrapMethodRef m
        put $ (fromIntegral $ length $ bootstrapArguments m :: Word16)
        forM_ (bootstrapArguments m) put -- Don't use the default list instance as it prepends the length
    get = do
        ref <- get
        numArgs <- get :: Get Word16
        args <- replicateM (fromIntegral numArgs) get
        return $ BootstrapMethod { bootstrapMethodRef = ref, bootstrapArguments = args }

instance AttributeConvertible BootstrapMethodsAttribute where
    toAttribute (BootstrapMethodsAttribute index methods) =
        Attribute { attributeName = index, attributeLength = payloadLength, attributeValue = payload }
        where numMethods = genericLength methods :: Word16
              payload = encode numMethods <> mconcat (map encode methods)
              payloadLength = fromIntegral $ B.length payload

    fromAttribute (Attribute name _ payload) = do
        (numMethods, payload') <- decodeOrFail' payload :: Either String (Word16, B.ByteString)
        let helper :: St.StateT B.ByteString (Except String) BootstrapMethod
            helper = do
                p <- St.get
                (m, p') <- liftEither $ decodeOrFail' p
                St.put p'
                return m
        methods <- runExcept $ St.evalStateT (replicateM (fromIntegral numMethods) helper) payload'
        return $ BootstrapMethodsAttribute { attributeNameIndex = name, attributeMethods = methods }

class HasAttributes a where
  attributes :: a stage -> Attributes stage

instance HasAttributes Class where
  attributes = classAttributes

instance HasAttributes Field where
  attributes = fieldAttributes

instance HasAttributes Method where
  attributes = methodAttributes
