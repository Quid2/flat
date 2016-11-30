{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Flat.Bits(Bits(..),bits,bools) where

import           Data.Bits                      hiding (Bits)
-- import           Data.ByteString.Lazy           (ByteString, toStrict, unpack)
import qualified Data.ByteString.Lazy           as L
import           Data.Flat.Class
import           Data.Flat.Filler
import           Data.Flat.Run
import qualified Data.Vector.Unboxed            as V
-- import           Data.Word
import           Text.PrettyPrint.HughesPJClass

--import           Data.Flat.Instances
--x = pPrint $ bits ()
--y = pPrint $ bits (True,False,True,True,False,True,True,True)
--z = pPrint $ bits (True,False,True,True,False,True,True,True,False)

type Bits = V.Vector Bool

bools :: Flat a => a -> [Bool]
bools = V.toList . bits

bits :: forall a. Flat a => a -> Bits
bits v = let e = flat v
             bs = e
             Right (PostAligned _ f) = unflatRaw e :: Decoded (PostAligned a)
             numBits = 8 * L.length bs - fillerLength f
         in V.generate (fromIntegral numBits) (\n -> let (bb,b) = n `divMod` 8 in testBit (L.index bs (fromIntegral bb)) (7-b))

bytes [] = []
bytes l = let (w,r) = splitAt 8 l in w : bytes r

instance Pretty Bits where pPrint = hsep . map prettyBits . bytes .  V.toList

prettyBits l = text . take (length l) . concatMap (\b -> if b then "1" else "0") $ l

-- Useless
-- instance Binary BitVector where
--   encode = encode . V.toList
--   decode = V.fromList <$> decode

-- instance HasModel a => HasModel (V.Vector a) where envType _ = envType (Proxy::Proxy (Q.List a))
-- instance (V.Unbox a,Hashable a) => Hashable (V.Vector a) where hashWithSalt s = hashWithSalt s . V.toList

-- |A sequence of bits
-- data Bits = Bits
--   [Word8]
--   Int -- num of valid bits in last byte
--   deriving Show

-- instance Pretty Bits where
--   pPrint (Bits bs n) = hsep (map pPrint . init $ bs) ++ [text . take n . prettyWord8 . last $ bs]

-- |Convert a value to Bits
-- bits :: forall a. Flat a => a -> Bits
-- bits v = let e = flat v
--              bs = L.unpack e
--              Right (PostAligned _ f) = unflatRaw e :: Decoded (PostAligned a)
--              (d,m) = fillerLength f `divMod` 8
--          in Bits (take (length bs - d) bs) (8-m)

-- partBits l n = let [b] = L.unpack l
--                in Bits [B.shiftL b n] (8-n) -- wrong length
