{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Flat.Bits(Bits(..),bits) where

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
  pPrint (Bits bs n) = char '<' <> (text . concat $ (map prettyWord8 . init $ bs) ++ [take n . prettyWord8 . last $ bs]) <> char '>'

-- |Convert a value to Bits
bits :: forall a. Flat a => a -> Bits
bits v = let e = flat v
             bs = L.unpack e
             Right (PostAligned _ f) = unflatRaw e :: Decoded (PostAligned a)
             (d,m) = fillerLength f `divMod` 8
         in Bits (take (length bs - d) bs) (8-m)

-- partBits l n = let [b] = L.unpack l
--                in Bits [B.shiftL b n] (8-n) -- wrong length
