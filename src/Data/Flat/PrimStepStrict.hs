module Data.Flat.Prim(Encoding,(<>),(<+>),(<|),Step(..),encodersR,mempty,bitEncoder,eBitsS,eTrueF,eFalseF,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where

import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes
import           Data.Monoid
import           Data.Foldable
-- Strict encoder, calculate max length of encoding then alloc single buffer and encode unsafely.
-- Faster than Flat on anything but lN3
type Encoding = Step

instance Monoid Step where
  {-# INLINE mempty #-}
  mempty = Step 0 return

  {-# INLINE mappend #-}
  mappend (Step n f) (Step n2 g) = Step (n+n2) (f >=> g)

  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

bitEncoder :: Encoding -> L.ByteString
bitEncoder (Step numBits op) = L.fromStrict $ bitEncoderStrict numBits op

-- bitEncoderL :: Step -> L.ByteString
-- bitEncoderL (Step numBits op) = bitEncoderLazy (numBits `div` 8 +1) (\(E fp s) op -> op s >>= done) op

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
encodersR :: [Step] -> Step
encodersR ws = mconcat . reverse $ ws -- without the explicit parameter the rules won't fire
-- encodersR ws = error $ unwords ["encodersR CALLED",show ws]

{-# INLINE (<+>) #-}
(<+>) :: Step -> Step -> Step
(<+>) = (<>)

infixr 5 <|

{-# INLINE (<|) #-}
(<|) = (<+>)

eListElem = eListElemS
eChar = eCharS
eUTS16 = eUTS16S
eBytes = eBytesS
eLazyBytes = eLazyBytesS
eShortBytes =  eShortBytesS
eNatural =  eNaturalS
eFloat = eFloatS
eDouble = eDoubleS
eInteger =  eIntegerS
eInt64 =  eInt64S
eInt32 =  eInt32S
eInt16 =  eInt16S
eInt8 = eInt8S
eInt = eIntS
eWord64 =  eWord64S
eWord32 =  eWord32S
eWord16 =  eWord16S
eWord8 = eWord8S
eWord = eWordS
eBits = eBitsS
eFiller = eFillerS
eBool = eBoolS
eTrue =  eTrueS
eFalse =  eFalseS

