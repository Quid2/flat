{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Flat.Bits where

import           Data.ByteString.Lazy           (ByteString, toStrict, unpack)
import qualified Data.ByteString.Lazy           as L
import           Data.Flat.Class
import           Data.Flat.Filler
import           Data.Flat.Run
import           Data.Word
import           Text.PrettyPrint.HughesPJClass

-- |A sequence of bits
data Bits = Bits
  [Word8]
  Int -- num of valid bits in last byte
  deriving Show

instance Pretty Bits where
  pPrint (Bits bs n) = text . concat $ (map showBits . init $ bs) ++ [take n . showBits . last $ bs]

-- |Convert a value to Bits
bits :: forall a. Flat a => a -> Bits
bits v = let e = flat . postAligned $ v
             bs = L.unpack e
             Right (PostAligned v' f) = unflat e :: Decoded (PostAligned a)
             (d,m) = fillerLength f `divMod` 8
         in Bits (take (length bs - d) bs) (8-m)

-- partBits l n = let [b] = L.unpack l
--                in Bits [B.shiftL b n] (8-n) -- wrong length
