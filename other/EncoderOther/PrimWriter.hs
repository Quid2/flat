module Data.Flat.Prim(Encoding,(<>),(<+>),(<|),Step(..),encodersR,encodersS,mempty,bitEncoder,eBitsS,eTrueF,eFalseF,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where

import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes
import           Data.Monoid hiding ((<>))
import           Data.Semigroup
import           Data.Foldable
import Data.Word
-- Strict encoder, calculate max length of encoding then alloc single buffer and encode unsafely.
-- Faster than Flat on anything but lN3
type Encoding = Writer

newtype Writer = Writer (S-> IO S)
instance Show Writer where show _ = "Writer"

instance Semigroup Writer where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  mappend (Writer f) (Writer g) = Writer (f >=> g)

  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

bitEncoder :: Encoding -> L.ByteString
--bitEncoder (Writer numBits op) = L.fromStrict $ bitEncoderStrict numBits op
bitEncoder (Writer op) = let size =20000000 in L.fromStrict $ bitEncoderStrict size op

-- bitEncoderL :: Writer -> L.ByteString
-- bitEncoderL (Writer numBits op) = bitEncoderLazy (numBits `div` 8 +1) (\(E fp s) op -> op s >>= done) op

{-# RULES
"encodersR12" forall a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12. encodersR [a12,a11,a10,a9,a8,a7,a6,a5,a4,a3,a2,a1] = a1 <> a2 <> a3 <> a4 <> a5 <> a6 <> a7 <> a8 <> a9 <> a10 <> a11 <> a12
"encodersR6" forall a1 a2 a3 a4 a5 a6. encodersR [a6,a5,a4,a3,a2,a1] = a1 <> a2 <> a3 <> a4 <> a5 <> a6
"encodersR5" forall a b c d e. encodersR [e,d,c,b,a] = a <> b <> c <> d <> e
"encodersR4" forall a b c d. encodersR [d,c,b,a] = a <> b <> c <> d
"encodersR3" forall b c d. encodersR [d,c,b] = b <> c <> d
"encodersR2" forall c d. encodersR [d,c] = c <> d
"encodersR1" forall d. encodersR [d] = d
"encodersR0" encodersR [] = mempty
#-}

{-# INLINE [0] encodersR #-}
encodersR :: [Writer] -> Writer
-- encodersR ws = mconcat . reverse $ ws -- without the explicit parameter the rules won't fire
encodersR ws = error $ unwords ["encodersR CALLED",show ws]

{- RULES
"encodersS12" forall a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12. encodersS [a12,a11,a10,a9,a8,a7,a6,a5,a4,a3,a2,a1] = a1 + stepSize a2 + stepSize a3 + stepSize a4 + stepSize a5 + stepSize a6 + stepSize a7 + stepSize a8 + stepSize a9 + stepSize a10 + stepSize a11 + stepSize a12
"encodersS6" forall a1 a2 a3 a4 a5 a6. encodersS [a6,a5,a4,a3,a2,a1] = a1 + stepSize a2 + stepSize a3 + stepSize a4 + stepSize a5 + stepSize a6
"encodersS5" forall a b c d e. encodersS [e,d,c,b,a] = a + stepSize b + stepSize c + stepSize d + stepSize e
"encodersS4" forall a b c d. encodersS [d,c,b,a] = a + stepSize b + stepSize c + stepSize d
"encodersS3" forall b c d. encodersS [d,c,b] = b + stepSize  c + stepSize d
"encodersS2" forall c d. encodersS [d,c] = stepSize c + stepSize d
"encodersS1" forall d. encodersS [d] = stepSize d
"encodersS0" encodersS [] = 0
-}


{-# INLINE [0] encodersS #-}
encodersS :: [Writer] -> Int
-- encodersS ws = foldl' (\n s -> stepSize s + n) 0 $ ws -- without the explicit parameter the rules won't fire
encodersS ws = error $ unwords ["encodersS CALLED",show ws]


{-# INLINE (<+>) #-}
(<+>) :: Writer -> Writer -> Writer
(<+>) = (<>)

infixr 5 <|

{-# INLINE (<|) #-}
(<|) = (<+>)

wprim = Writer

eFalse,eTrue::Writer
eBool::Bool->Writer
eFiller::Writer
-- eBits :: 
eUnsigned64 :: Word64 -> Writer
eUnsigned32 :: Word32 -> Writer
eUnsigned16 :: Word16 -> Writer
eUnsigned :: Integer -> Writer

-- eBitsS = eBits
eListElem (Writer e) = Writer $ eTrueF >=> e
--eListElem e = eTrue <> e
eUTF16 = Writer . eUTF16F
eBytes = Writer . eBytesF
eLazyBytes = Writer . eLazyBytesF
eShortBytes = Writer. eShortBytesF
eUnsigned = Writer. eUnsignedF
eUnsigned64 = Writer. varWordF
eUnsigned32 = Writer. varWordF
eUnsigned16 = Writer. varWordF
eWord32BE = Writer . eWord32BEF
eWord64BE = Writer. eWord64BEF
eWord8 = Writer . eWord8F
eBits n f = Writer $ eBitsF n f
eFiller = Writer eFillerF
eBool = Writer . eBoolF
eTrue = Writer eTrueF
eFalse = Writer eFalseF
