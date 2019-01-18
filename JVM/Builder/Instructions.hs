{-# LANGUAGE FlexibleContexts #-}
-- | This module exports shortcuts for some of JVM instructions (which are defined in JVM.Assembler).
-- These functions get Constants, put them into constants pool and generate instruction using index
-- of constant in the pool.
module JVM.Builder.Instructions where

import           Codec.Binary.UTF8.String (encodeString)
import qualified Data.ByteString.Lazy     as B
import           Data.String
import           Data.Word

import           JVM.Assembler
import           JVM.Builder.Monad
import           JVM.ClassFile

nop :: MonadGenerator m => m ()
nop = i0 NOP
aconst_null :: MonadGenerator m => m ()
aconst_null = i0 ACONST_NULL
iconst_m1 :: MonadGenerator m => m ()
iconst_m1 = i0 ICONST_M1
iconst_0 :: MonadGenerator m => m ()
iconst_0 = i0 ICONST_0
iconst_1 :: MonadGenerator m => m ()
iconst_1 = i0 ICONST_1
iconst_2 :: MonadGenerator m => m ()
iconst_2 = i0 ICONST_2
iconst_3 :: MonadGenerator m => m ()
iconst_3 = i0 ICONST_3
iconst_4 :: MonadGenerator m => m ()
iconst_4 = i0 ICONST_4
iconst_5 :: MonadGenerator m => m ()
iconst_5 = i0 ICONST_5
lconst_0 :: MonadGenerator m => m ()
lconst_0 = i0 LCONST_0
lconst_1 :: MonadGenerator m => m ()
lconst_1 = i0 LCONST_1
fconst_0 :: MonadGenerator m => m ()
fconst_0 = i0 FCONST_0
fconst_1 :: MonadGenerator m => m ()
fconst_1 = i0 FCONST_1
fconst_2 :: MonadGenerator m => m ()
fconst_2 = i0 FCONST_2
dconst_0 :: MonadGenerator m => m ()
dconst_0 = i0 DCONST_0
dconst_1 :: MonadGenerator m => m ()
dconst_1 = i0 DCONST_1

bipush :: MonadGenerator m => Word8 -> m ()
bipush x = i0 (BIPUSH x)
sipush :: MonadGenerator m => Word16 -> m ()
sipush x = i0 (SIPUSH x)

ldc1 :: MonadGenerator m => Constant Direct -> m ()
ldc1 x = i8 LDC1 x
ldc2 :: MonadGenerator m => Constant Direct -> m ()
ldc2 x = i1 LDC2 x
ldc2w :: MonadGenerator m => Constant Direct -> m ()
ldc2w x = i1 LDC2W x
iload :: MonadGenerator m => Word8 -> m ()
iload x = i0 (ILOAD x)
iloadw :: MonadGenerator m => Word16 -> m ()
iloadw = wide ILOAD
lload :: MonadGenerator m => Word8 -> m ()
lload x = i0 (LLOAD x)
lloadw :: MonadGenerator m => Word16 -> m ()
lloadw = wide LLOAD
fload :: MonadGenerator m => Word8 -> m ()
fload x = i0 (FLOAD x)
floadw :: MonadGenerator m => Word16 -> m ()
floadw = wide FLOAD
dload :: MonadGenerator m => Word8 -> m ()
dload x = i0 (DLOAD x)
dloadw :: MonadGenerator m => Word16 -> m ()
dloadw = wide DLOAD
aload :: MonadGenerator m => Word8 -> m ()
aload x = i0 (ALOAD x)
aloadw :: MonadGenerator m => Word16 -> m ()
aloadw = wide ALOAD

iload_ :: MonadGenerator m => IMM -> m ()
iload_ = i0 . ILOAD_
lload_ :: MonadGenerator m => IMM -> m ()
lload_ = i0 . LLOAD_
fload_ :: MonadGenerator m => IMM -> m ()
fload_ = i0 . FLOAD_
dload_ :: MonadGenerator m => IMM -> m ()
dload_ = i0 . DLOAD_
aload_ :: MonadGenerator m => IMM -> m ()
aload_ = i0 . ALOAD_

iaload :: MonadGenerator m => m ()
iaload = i0 IALOAD
laload :: MonadGenerator m => m ()
laload = i0 LALOAD
faload :: MonadGenerator m => m ()
faload = i0 FALOAD
daload :: MonadGenerator m => m ()
daload = i0 DALOAD
aaload :: MonadGenerator m => m ()
aaload = i0 AALOAD
caload :: MonadGenerator m => m ()
caload = i0 CALOAD
saload :: MonadGenerator m => m ()
saload = i0 SALOAD

istore :: MonadGenerator m => Word8 -> m ()
istore = i0 . ISTORE
istorew :: MonadGenerator m => Word16 -> m ()
istorew = wide ISTORE
lstore :: MonadGenerator m => Word8 -> m ()
lstore = i0 . LSTORE
lstorew :: MonadGenerator m => Word16 -> m ()
lstorew = wide LSTORE
fstore :: MonadGenerator m => Word8 -> m ()
fstore = i0 . FSTORE
fstorew :: MonadGenerator m => Word16 -> m ()
fstorew = wide FSTORE
dstore :: MonadGenerator m => Word8 -> m ()
dstore = i0 . DSTORE
dstorew :: MonadGenerator m => Word16 -> m ()
dstorew = wide DSTORE
astore :: MonadGenerator m => Word8 -> m ()
astore = i0 . ASTORE
astorew :: MonadGenerator m => Word16 -> m ()
astorew = wide ASTORE

istore_ :: MonadGenerator m => IMM -> m ()
istore_ = i0 . ISTORE_
lstore_ :: MonadGenerator m => IMM -> m ()
lstore_ = i0 . LSTORE_
fstore_ :: MonadGenerator m => IMM -> m ()
fstore_ = i0 . FSTORE_
dstore_ :: MonadGenerator m => IMM -> m ()
dstore_ = i0 . DSTORE_
astore_ :: MonadGenerator m => IMM -> m ()
astore_ = i0 . ASTORE_

iastore :: MonadGenerator m => m ()
iastore = i0 IASTORE
lastore :: MonadGenerator m => m ()
lastore = i0 LASTORE
fastore :: MonadGenerator m => m ()
fastore = i0 FASTORE
dastore :: MonadGenerator m => m ()
dastore = i0 DASTORE
aastore :: MonadGenerator m => m ()
aastore = i0 AASTORE
bastore :: MonadGenerator m => m ()
bastore = i0 BASTORE
castore :: MonadGenerator m => m ()
castore = i0 CASTORE
sastore :: MonadGenerator m => m ()
sastore = i0 SASTORE

pop :: MonadGenerator m => m ()
pop     = i0 POP
pop2 :: MonadGenerator m => m ()
pop2    = i0 POP2
dup :: MonadGenerator m => m ()
dup     = i0 DUP
dup_x1 :: MonadGenerator m => m ()
dup_x1  = i0 DUP_X1
dup_x2 :: MonadGenerator m => m ()
dup_x2  = i0 DUP_X2
dup2 :: MonadGenerator m => m ()
dup2    = i0 DUP2
dup2_x1 :: MonadGenerator m => m ()
dup2_x1 = i0 DUP2_X1
dup2_x2 :: MonadGenerator m => m ()
dup2_x2 = i0 DUP2_X2
swap :: MonadGenerator m => m ()
swap    = i0 SWAP
iadd :: MonadGenerator m => m ()
iadd    = i0 IADD
ladd :: MonadGenerator m => m ()
ladd    = i0 LADD
fadd :: MonadGenerator m => m ()
fadd    = i0 FADD
dadd :: MonadGenerator m => m ()
dadd    = i0 DADD
isub :: MonadGenerator m => m ()
isub    = i0 ISUB
lsub :: MonadGenerator m => m ()
lsub    = i0 LSUB
fsub :: MonadGenerator m => m ()
fsub    = i0 FSUB
dsub :: MonadGenerator m => m ()
dsub    = i0 DSUB
imul :: MonadGenerator m => m ()
imul    = i0 IMUL
lmul :: MonadGenerator m => m ()
lmul    = i0 LMUL
fmul :: MonadGenerator m => m ()
fmul    = i0 FMUL
dmul :: MonadGenerator m => m ()
dmul    = i0 DMUL
idiv :: MonadGenerator m => m ()
idiv    = i0 IDIV
ldiv :: MonadGenerator m => m ()
ldiv    = i0 LDIV
fdiv :: MonadGenerator m => m ()
fdiv    = i0 FDIV
ddiv :: MonadGenerator m => m ()
ddiv    = i0 DDIV
irem :: MonadGenerator m => m ()
irem    = i0 IREM
lrem :: MonadGenerator m => m ()
lrem    = i0 LREM
frem :: MonadGenerator m => m ()
frem    = i0 FREM
drem :: MonadGenerator m => m ()
drem    = i0 DREM
ineg :: MonadGenerator m => m ()
ineg    = i0 INEG
lneg :: MonadGenerator m => m ()
lneg    = i0 LNEG
fneg :: MonadGenerator m => m ()
fneg    = i0 FNEG
dneg :: MonadGenerator m => m ()
dneg    = i0 DNEG
ishl :: MonadGenerator m => m ()
ishl    = i0 ISHL
lshl :: MonadGenerator m => m ()
lshl    = i0 LSHL
ishr :: MonadGenerator m => m ()
ishr    = i0 ISHR
lshr :: MonadGenerator m => m ()
lshr    = i0 LSHR
iushr :: MonadGenerator m => m ()
iushr   = i0 IUSHR
lushr :: MonadGenerator m => m ()
lushr   = i0 LUSHR
iand :: MonadGenerator m => m ()
iand    = i0 IAND
land :: MonadGenerator m => m ()
land    = i0 LAND
ior :: MonadGenerator m => m ()
ior     = i0 IOR
lor :: MonadGenerator m => m ()
lor     = i0 LOR
ixor :: MonadGenerator m => m ()
ixor    = i0 IXOR
lxor :: MonadGenerator m => m ()
lxor    = i0 LXOR

iinc :: MonadGenerator m => Word8 -> Word8 -> m ()
iinc x y = i0 (IINC x y)

i2l :: MonadGenerator m => m ()
i2l  = i0 I2L
i2f :: MonadGenerator m => m ()
i2f  = i0 I2F
i2d :: MonadGenerator m => m ()
i2d  = i0 I2D
l2i :: MonadGenerator m => m ()
l2i  = i0 L2I
l2f :: MonadGenerator m => m ()
l2f  = i0 L2F
l2d :: MonadGenerator m => m ()
l2d  = i0 L2D
f2i :: MonadGenerator m => m ()
f2i  = i0 F2I
f2l :: MonadGenerator m => m ()
f2l  = i0 F2L
f2d :: MonadGenerator m => m ()
f2d  = i0 F2D
d2i :: MonadGenerator m => m ()
d2i  = i0 D2I
d2l :: MonadGenerator m => m ()
d2l  = i0 D2L
d2f :: MonadGenerator m => m ()
d2f  = i0 D2F
i2b :: MonadGenerator m => m ()
i2b  = i0 I2B
i2c :: MonadGenerator m => m ()
i2c  = i0 I2C
i2s :: MonadGenerator m => m ()
i2s  = i0 I2S
lcmp :: MonadGenerator m => m ()
lcmp = i0 LCMP

-- | Wide instruction using constant pool
widec :: MonadGenerator m => (Word8 -> Instruction) -> Constant Direct -> m ()
widec fn c = do
  ix <- addToPool c
  wide fn ix
wide :: MonadGenerator m => (Word8 -> Instruction) -> Word16 -> m ()
wide fn c = do
  let ix0 = fromIntegral (c `div` 0x100) :: Word8
      ix1 = fromIntegral (c `mod` 0x100) :: Word8
  i0 (WIDE ix0 $ fn ix1)

new :: MonadGenerator m => B.ByteString -> m ()
new cls =
  i1 NEW (CClass cls)

checkCast :: MonadGenerator m => B.ByteString -> m ()
checkCast cls = i1 CHECKCAST (CClass cls)

newArray :: MonadGenerator m => ArrayType -> m ()
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray :: MonadGenerator m => B.ByteString -> m ()
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual :: MonadGenerator m => B.ByteString -> NameType (Method Direct) -> m ()
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic :: MonadGenerator m => B.ByteString -> NameType (Method Direct) -> m ()
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial :: MonadGenerator m => B.ByteString -> NameType (Method Direct) -> m ()
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

invokeDynamic :: MonadGenerator m => Word16 -> NameType (Method Direct) -> m ()
invokeDynamic bootstrapIndex sig =
  i1 INVOKEDYNAMIC (CInvokeDynamic bootstrapIndex sig)

getStaticField :: MonadGenerator m => B.ByteString -> NameType (Field Direct) -> m ()
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

putStaticField :: MonadGenerator m => B.ByteString -> NameType (Field Direct) -> m ()
putStaticField cls sig =
    i1 PUTSTATIC (CField cls sig)

loadString :: MonadGenerator m => String -> m ()
loadString str =
  i8 LDC1 (CString $ fromString $ encodeString $ str)

allocArray :: MonadGenerator m => B.ByteString -> m ()
allocArray cls =
  i1 ANEWARRAY (CClass cls)

throw :: MonadGenerator m => m ()
throw = i0 ATHROW