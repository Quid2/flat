{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |Utilities to represent and display bit sequences
module Data.Flat.Bits (Bits, toBools, bits, paddedBits, asBytes) where

import           Data.Bits                      hiding (Bits)
import qualified Data.ByteString           as L
import           Data.Flat.Decoder
import           Data.Flat.Class
import           Data.Flat.Filler
import           Data.Flat.Run
-- import           Data.Int
import qualified Data.Vector.Unboxed            as V
import           Data.Word
import           Text.PrettyPrint.HughesPJClass

-- |A sequence of bits
type Bits = V.Vector Bool

toBools :: Bits -> [Bool]
toBools = V.toList

-- |The sequence of bits corresponding to the serialization of the passed value (without any final byte padding)
bits :: forall a. Flat a => a -> Bits
bits v = let lbs = flat v
             Right (PostAligned _ f) = unflatRaw lbs :: Decoded (PostAligned a)
         in takeBits (8 * L.length lbs - fillerLength f) lbs

-- |The sequence of bits corresponding to the byte-padded serialization of the passed value
paddedBits :: forall a. Flat a => a -> Bits
paddedBits v = let lbs = flat v
               in takeBits (8 * L.length lbs) lbs

takeBits :: Int -> L.ByteString -> V.Vector Bool
takeBits numBits lbs  = V.generate (fromIntegral numBits) (\n -> let (bb,b) = n `divMod` 8 in testBit (L.index lbs (fromIntegral bb)) (7-b))

-- |Convert a sequence of bits to the corresponding list of bytes
asBytes :: Bits -> [Word8]
asBytes = map byteVal . bytes .  V.toList

-- |Convert to the corresponding value (most significant bit first)
byteVal :: [Bool] -> Word8
byteVal = sum . map (\(e,b) -> if b then e else 0). zip [2 ^ n | n <- [7::Int,6..0]]

-- |Split a list in groups of 8 elements or less
bytes :: [t] -> [[t]]
bytes [] = []
bytes l  = let (w,r) = splitAt 8 l in w : bytes r

instance Pretty Bits where pPrint = hsep . map prettyBits . bytes .  V.toList

prettyBits :: Foldable t => t Bool -> Doc
prettyBits l = text . take (length l) . concatMap (\b -> if b then "1" else "0") $ l

