{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module JVM.Attributes.InnerClasses where

import Data.Word
import Data.BinaryState
import Control.Monad
import qualified Data.ByteString.Lazy as B
import JVM.BitMask.BitMask

data InnerClasses = InnerClasses {
  icsNumberOfClasses :: Word16,
  icsClasses :: [ClassEntry]
}

data ClassEntry = ClassEntry {
  innerClassInfoIndex :: Word16,
  outerClassInfoIndex :: Word16,
  innerNameIndex :: Word16,
  innerClassAccessFlag :: [NestedClassFlag]
}

instance BinaryState Integer InnerClasses where
  put (InnerClasses {..}) = do
    put icsNumberOfClasses
    forM_ icsClasses put

  get = do
    numberOfClasses <- get
    classes <- replicateM (fromIntegral numberOfClasses) get
    return $ InnerClasses numberOfClasses classes


instance BinaryState Integer ClassEntry where
  put (ClassEntry {..}) = do
    put innerClassInfoIndex
    put outerClassInfoIndex
    put innerNameIndex
    put $ (maskValue innerClassAccessFlag :: Word16)

  get = do
    innerIndex <- get
    outerIndex <- get
    nameIndex <- get
    flagValue <- get
    return $ ClassEntry innerIndex outerIndex nameIndex (toMask (flagValue :: Word16))


data NestedClassFlag =
      ACC_PUBLIC       -- ^ 0x0001 Visible for all
    | ACC_PRIVATE      -- ^ 0x0002 Visible only for defined class
    | ACC_PROTECTED    -- ^ 0x0004 Visible only for subclasses
    | ACC_STATIC       -- ^ 0x0008 Static method or variable
    | ACC_FINAL        -- ^ 0x0010 No further subclassing or assignments

    | ACC_INTERFACE    -- ^ 0x0200 Class is interface
    | ACC_ABSTRACT     -- ^ 0x0400

    | ACC_SYNTHETIC     -- ^ 0x1000
    | ACC_ANNOTATION    -- ^ 0x2000
    | ACC_ENUM          -- ^ 0x4000
    deriving (Eq, Show, Ord, Enum)

instance BitMask NestedClassFlag where
  maskBit x = case x of
    ACC_PUBLIC       -> 0
    ACC_PRIVATE      -> 1
    ACC_PROTECTED    -> 2
    ACC_STATIC       -> 3
    ACC_FINAL        -> 4
    ACC_INTERFACE    -> 9
    ACC_ABSTRACT     -> 10
    ACC_SYNTHETIC    -> 12
    ACC_ANNOTATION   -> 13
    ACC_ENUM         -> 14

decodeInnerClasses :: B.ByteString -> InnerClasses
decodeInnerClasses = decodeS (0 :: Integer)

encodeInnerClasses :: InnerClasses -> B.ByteString
encodeInnerClasses = encodeS (0 :: Integer)
