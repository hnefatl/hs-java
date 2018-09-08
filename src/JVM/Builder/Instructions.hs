{-# LANGUAGE FlexibleContexts #-}
-- | This module exports shortcuts for some of JVM instructions (which are defined in JVM.Assembler).
-- These functions get Constants, put them into constants pool and generate instruction using index
-- of constant in the pool.
module JVM.Builder.Instructions where

import Data.Word
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String (encodeString)
import Data.String
import Control.Monad.State
import JVM.ClassFile
import JVM.Assembler
import JVM.Builder.Monad

nop :: (Monad m,  MonadState GState m) => m ()
nop = i0 NOP
aconst_null :: (Monad m,  MonadState GState m) => m ()
aconst_null = i0 ACONST_NULL
iconst_m1 :: (Monad m,  MonadState GState m) => m ()
iconst_m1 = i0 ICONST_M1
iconst_0 :: (Monad m,  MonadState GState m) => m ()
iconst_0 = i0 ICONST_0
iconst_1 :: (Monad m,  MonadState GState m) => m ()
iconst_1 = i0 ICONST_1
iconst_2 :: (Monad m,  MonadState GState m) => m ()
iconst_2 = i0 ICONST_2
iconst_3 :: (Monad m,  MonadState GState m) => m ()
iconst_3 = i0 ICONST_3
iconst_4 :: (Monad m,  MonadState GState m) => m ()
iconst_4 = i0 ICONST_4
iconst_5 :: (Monad m,  MonadState GState m) => m ()
iconst_5 = i0 ICONST_5
lconst_0 :: (Monad m,  MonadState GState m) => m ()
lconst_0 = i0 LCONST_0
lconst_1 :: (Monad m,  MonadState GState m) => m ()
lconst_1 = i0 LCONST_1
fconst_0 :: (Monad m,  MonadState GState m) => m ()
fconst_0 = i0 FCONST_0
fconst_1 :: (Monad m,  MonadState GState m) => m ()
fconst_1 = i0 FCONST_1
fconst_2 :: (Monad m,  MonadState GState m) => m ()
fconst_2 = i0 FCONST_2
dconst_0 :: (Monad m,  MonadState GState m) => m ()
dconst_0 = i0 DCONST_0
dconst_1 :: (Monad m,  MonadState GState m) => m ()
dconst_1 = i0 DCONST_1

bipush :: (Monad m,  MonadState GState m) => Word8 -> m ()
bipush x = i0 (BIPUSH x)
sipush :: (Monad m,  MonadState GState m) => Word16 -> m ()
sipush x = i0 (SIPUSH x)

ldc1 :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
ldc1 x = i8 LDC1 x
ldc2 :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
ldc2 x = i1 LDC2 x
ldc2w :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
ldc2w x = i1 LDC2W x
iload :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
iload x = i8 ILOAD x
lload :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
lload x = i8 LLOAD x
fload :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
fload x = i8 FLOAD x
dload :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
dload x = i8 DLOAD x
aload :: (Monad m,  MonadState GState m) => Constant Direct -> m ()
aload x = i8 ALOAD x

iload_ :: (Monad m,  MonadState GState m) => IMM -> m ()
iload_ x = i0 (ILOAD_ x)
lload_ :: (Monad m,  MonadState GState m) => IMM -> m ()
lload_ x = i0 (LLOAD_ x)
fload_ :: (Monad m,  MonadState GState m) => IMM -> m ()
fload_ x = i0 (FLOAD_ x)
dload_ :: (Monad m,  MonadState GState m) => IMM -> m ()
dload_ x = i0 (DLOAD_ x)
aload_ :: (Monad m,  MonadState GState m) => IMM -> m ()
aload_ x = i0 (ALOAD_ x)

iaload :: (Monad m,  MonadState GState m) => m ()
iaload = i0 IALOAD
laload :: (Monad m,  MonadState GState m) => m ()
laload = i0 LALOAD
faload :: (Monad m,  MonadState GState m) => m ()
faload = i0 FALOAD
daload :: (Monad m,  MonadState GState m) => m ()
daload = i0 DALOAD
aaload :: (Monad m,  MonadState GState m) => m ()
aaload = i0 AALOAD
caload :: (Monad m,  MonadState GState m) => m ()
caload = i0 CALOAD
saload :: (Monad m,  MonadState GState m) => m ()
saload = i0 SALOAD

istore :: (Monad m, MonadState GState m) => Constant Direct -> m ()
istore x = i8 ISTORE x
lstore :: (Monad m, MonadState GState m) => Constant Direct -> m ()
lstore x = i8 LSTORE x
fstore :: (Monad m, MonadState GState m) => Constant Direct -> m ()
fstore x = i8 FSTORE x
dstore :: (Monad m, MonadState GState m) => Constant Direct -> m ()
dstore x = i8 DSTORE x
astore :: (Monad m, MonadState GState m) => Constant Direct -> m ()
astore x = i8 ASTORE x

istore_ :: (Monad m, MonadState GState m) => Word8 -> m ()
istore_ x = i0 (ISTORE x)
lstore_ :: (Monad m, MonadState GState m) => Word8 -> m ()
lstore_ x = i0 (LSTORE x)
fstore_ :: (Monad m, MonadState GState m) => Word8 -> m ()
fstore_ x = i0 (FSTORE x)
dstore_ :: (Monad m, MonadState GState m) => Word8 -> m ()
dstore_ x = i0 (DSTORE x)
astore_ :: (Monad m, MonadState GState m) => Word8 -> m ()
astore_ x = i0 (ASTORE x)

iastore :: (Monad m,  MonadState GState m) => m ()
iastore = i0 IASTORE
lastore :: (Monad m,  MonadState GState m) => m ()
lastore = i0 LASTORE
fastore :: (Monad m,  MonadState GState m) => m ()
fastore = i0 FASTORE
dastore :: (Monad m,  MonadState GState m) => m ()
dastore = i0 DASTORE
aastore :: (Monad m,  MonadState GState m) => m ()
aastore = i0 AASTORE
bastore :: (Monad m,  MonadState GState m) => m ()
bastore = i0 BASTORE
castore :: (Monad m,  MonadState GState m) => m ()
castore = i0 CASTORE
sastore :: (Monad m,  MonadState GState m) => m ()
sastore = i0 SASTORE

pop :: (Monad m,  MonadState GState m) => m ()
pop     = i0 POP
pop2 :: (Monad m,  MonadState GState m) => m ()
pop2    = i0 POP2
dup :: (Monad m,  MonadState GState m) => m ()
dup     = i0 DUP
dup_x1 :: (Monad m,  MonadState GState m) => m ()
dup_x1  = i0 DUP_X1
dup_x2 :: (Monad m,  MonadState GState m) => m ()
dup_x2  = i0 DUP_X2
dup2 :: (Monad m,  MonadState GState m) => m ()
dup2    = i0 DUP2
dup2_x1 :: (Monad m,  MonadState GState m) => m ()
dup2_x1 = i0 DUP2_X1
dup2_x2 :: (Monad m,  MonadState GState m) => m ()
dup2_x2 = i0 DUP2_X2
swap :: (Monad m,  MonadState GState m) => m ()
swap    = i0 SWAP
iadd :: (Monad m,  MonadState GState m) => m ()
iadd    = i0 IADD
ladd :: (Monad m,  MonadState GState m) => m ()
ladd    = i0 LADD
fadd :: (Monad m,  MonadState GState m) => m ()
fadd    = i0 FADD
dadd :: (Monad m,  MonadState GState m) => m ()
dadd    = i0 DADD
isub :: (Monad m,  MonadState GState m) => m ()
isub    = i0 ISUB
lsub :: (Monad m,  MonadState GState m) => m ()
lsub    = i0 LSUB
fsub :: (Monad m,  MonadState GState m) => m ()
fsub    = i0 FSUB
dsub :: (Monad m,  MonadState GState m) => m ()
dsub    = i0 DSUB
imul :: (Monad m,  MonadState GState m) => m ()
imul    = i0 IMUL
lmul :: (Monad m,  MonadState GState m) => m ()
lmul    = i0 LMUL
fmul :: (Monad m,  MonadState GState m) => m ()
fmul    = i0 FMUL
dmul :: (Monad m,  MonadState GState m) => m ()
dmul    = i0 DMUL
idiv :: (Monad m,  MonadState GState m) => m ()
idiv    = i0 IDIV
ldiv :: (Monad m,  MonadState GState m) => m ()
ldiv    = i0 LDIV
fdiv :: (Monad m,  MonadState GState m) => m ()
fdiv    = i0 FDIV
ddiv :: (Monad m,  MonadState GState m) => m ()
ddiv    = i0 DDIV
irem :: (Monad m,  MonadState GState m) => m ()
irem    = i0 IREM
lrem :: (Monad m,  MonadState GState m) => m ()
lrem    = i0 LREM
frem :: (Monad m,  MonadState GState m) => m ()
frem    = i0 FREM
drem :: (Monad m,  MonadState GState m) => m ()
drem    = i0 DREM
ineg :: (Monad m,  MonadState GState m) => m ()
ineg    = i0 INEG
lneg :: (Monad m,  MonadState GState m) => m ()
lneg    = i0 LNEG
fneg :: (Monad m,  MonadState GState m) => m ()
fneg    = i0 FNEG
dneg :: (Monad m,  MonadState GState m) => m ()
dneg    = i0 DNEG
ishl :: (Monad m,  MonadState GState m) => m ()
ishl    = i0 ISHL
lshl :: (Monad m,  MonadState GState m) => m ()
lshl    = i0 LSHL
ishr :: (Monad m,  MonadState GState m) => m ()
ishr    = i0 ISHR
lshr :: (Monad m,  MonadState GState m) => m ()
lshr    = i0 LSHR
iushr :: (Monad m,  MonadState GState m) => m ()
iushr   = i0 IUSHR
lushr :: (Monad m,  MonadState GState m) => m ()
lushr   = i0 LUSHR
iand :: (Monad m,  MonadState GState m) => m ()
iand    = i0 IAND
land :: (Monad m,  MonadState GState m) => m ()
land    = i0 LAND
ior :: (Monad m,  MonadState GState m) => m ()
ior     = i0 IOR
lor :: (Monad m,  MonadState GState m) => m ()
lor     = i0 LOR
ixor :: (Monad m,  MonadState GState m) => m ()
ixor    = i0 IXOR
lxor :: (Monad m,  MonadState GState m) => m ()
lxor    = i0 LXOR

iinc :: (Monad m, MonadState GState m) => Word8 -> Word8 -> m ()
iinc x y = i0 (IINC x y)

i2l :: (Monad m,  MonadState GState m) => m ()
i2l  = i0 I2L
i2f :: (Monad m,  MonadState GState m) => m ()
i2f  = i0 I2F
i2d :: (Monad m,  MonadState GState m) => m ()
i2d  = i0 I2D
l2i :: (Monad m,  MonadState GState m) => m ()
l2i  = i0 L2I
l2f :: (Monad m,  MonadState GState m) => m ()
l2f  = i0 L2F
l2d :: (Monad m,  MonadState GState m) => m ()
l2d  = i0 L2D
f2i :: (Monad m,  MonadState GState m) => m ()
f2i  = i0 F2I
f2l :: (Monad m,  MonadState GState m) => m ()
f2l  = i0 F2L
f2d :: (Monad m,  MonadState GState m) => m ()
f2d  = i0 F2D
d2i :: (Monad m,  MonadState GState m) => m ()
d2i  = i0 D2I
d2l :: (Monad m,  MonadState GState m) => m ()
d2l  = i0 D2L
d2f :: (Monad m,  MonadState GState m) => m ()
d2f  = i0 D2F
i2b :: (Monad m,  MonadState GState m) => m ()
i2b  = i0 I2B
i2c :: (Monad m,  MonadState GState m) => m ()
i2c  = i0 I2C
i2s :: (Monad m,  MonadState GState m) => m ()
i2s  = i0 I2S
lcmp :: (Monad m,  MonadState GState m) => m ()
lcmp = i0 LCMP

-- | Wide instruction
wide :: (Monad m, MonadState GState m) => (Word8 -> Instruction) -> Constant Direct -> m ()
wide fn c = do
  ix <- addToPool c
  let ix0 = fromIntegral (ix `div` 0x100) :: Word8
      ix1 = fromIntegral (ix `mod` 0x100) :: Word8
  i0 (WIDE ix0 $ fn ix1)

new :: (Monad m, MonadState GState m) => B.ByteString -> m ()
new cls =
  i1 NEW (CClass cls)

newArray :: (Monad m, MonadState GState m) => ArrayType -> m ()
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray :: (Monad m, MonadState GState m) => B.ByteString -> m ()
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual :: (Monad m, MonadState GState m) => B.ByteString -> NameType (Method Direct) -> m ()
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic :: (Monad m, MonadState GState m) => B.ByteString -> NameType (Method Direct) -> m ()
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial :: (Monad m, MonadState GState m) => B.ByteString -> NameType (Method Direct) -> m ()
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField :: (Monad m, MonadState GState m) => B.ByteString -> NameType (Field Direct) -> m ()
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

loadString :: (Monad m, MonadState GState m) => String -> m ()
loadString str =
  i8 LDC1 (CString $ fromString $ encodeString $ str)

allocArray :: (Monad m, MonadState GState m) => B.ByteString -> m ()
allocArray cls =
  i1 ANEWARRAY (CClass cls)

