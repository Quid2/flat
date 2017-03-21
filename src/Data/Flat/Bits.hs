{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |Utilities to represent and display bit sequences
module Data.Flat.Bits (
    Bits,
    --bits,
    -- bools,
    valueBits,
    asBytes,
    ) where

import           Data.Bits                      hiding (Bits)
import qualified Data.ByteString.Lazy           as L
import           Data.Flat.Class
import           Data.Flat.Filler
import           Data.Flat.Run
import           Data.Int
import qualified Data.Vector.Unboxed            as V
import           Data.Word
import           Text.PrettyPrint.HughesPJClass

--import           Data.Flat.Instances
-- x = pPrint $ bits ()
-- y = pPrint $ bits (True,False,True,True,False,True,True,True)
--z = pPrint $ bits (True,False,True,True,False,True,True,True,False)

-- |A sequence of bits
type Bits = V.Vector Bool

-- -- |The sequence of booleans correspo
-- bools :: Flat a => a -> [Bool]
-- bools = V.toList . valueBits

-- bits :: L.ByteString -> Bits
-- bits lbs = takeBits (8 * L.length lbs) lbs

-- |The sequence of bits corresponding to the serialization of the passed value (without any final byte padding)
valueBits :: forall a. Flat a => a -> Bits
valueBits v = let lbs = flat v
                  Right (PostAligned _ f) = unflatRaw lbs :: Decoded (PostAligned a)
              in takeBits (8 * L.length lbs - fillerLength f) lbs
  where
    takeBits :: Int64 -> L.ByteString -> V.Vector Bool
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

