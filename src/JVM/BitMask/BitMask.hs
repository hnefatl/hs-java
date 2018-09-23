module JVM.BitMask.BitMask where

import Data.Bits (xor, testBit, Bits)

class BitMask a where
  maskBit :: a -> Integer

maskFlagValue :: BitMask a => Num b => a -> b
maskFlagValue f = fromInteger (2 ^ (maskBit f))

maskValue :: BitMask a => Bits b => Num b => [a] -> b
maskValue = foldl (\v -> (xor v) . maskFlagValue) 0

toMask :: Enum a => BitMask a => Bits b => Num b => b -> [a]
toMask n =
    filterFalse =<< (mkPair <$> all)
    where
      filterFalse (_, False) = []
      filterFalse (x, _) = [x]
      all = enumFrom (toEnum 0)
      mkPair f = (f, testBit n (fromInteger $ maskBit f))

