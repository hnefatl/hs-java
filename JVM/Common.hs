{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Options_GHC -fno-warn-orphans #-}
-- | This module declares some commonly used functions and instances.
module JVM.Common
  (toCharList,
  showListIx,
  byteString,
  bimapMap
  ) where

import Control.DeepSeq (NFData, rnf)
import qualified Data.Bimap as Bimap
import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Bifunctor (second)
import           Data.Default

instance Default B.ByteString where
    def = mempty
instance (NFData a, NFData b) => NFData (Bimap.Bimap a b) where
    rnf = rnf . Bimap.toList

toCharList :: B.ByteString -> [Int]
toCharList bstr = map fromIntegral $ B.unpack bstr

showListIx :: (Show i, Show a) => [(i,a)] -> String
showListIx list = unlines $ map s list
  where s (i, x) = show i ++ ":\t" ++ show x

byteString ::  Binary t => t -> B.ByteString
byteString x = runPut (put x)

bimapMap :: (Ord a, Ord c) => (b -> c) -> Bimap.Bimap a b -> Bimap.Bimap a c
bimapMap f = Bimap.fromList . map (second f) . Bimap.toList