{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
module Data.Flat.Prim(Encoding,(<>),(<+>),mempty,bitEncoder,module Data.Flat.Pokes,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Encoding
import qualified Data.Flat.Pokes      as P
import           Data.Flat.Types
import           Data.Foldable

{-# INLINE (<+>) #-}
(<+>) = (<>)

-- Slower than Lazy Seq
bitEncoder :: Encoding -> L.ByteString
bitEncoder sop = L.fromStrict $ P.bitEncoderStrict (encLen sop) (encoder sop)

encoder e s = sfoldlM step s e
  where
    step !s !n = case n of
          Tag8 n t      -> P.eBitsF n t s
          TagByteFiller -> P.eFillerF s
          TagBool False -> P.eFalseF s
          TagBool True  -> P.eTrueF s

encLen :: Encoding -> Int
encLen = foldl' (\s e -> s + tagLen e) 0

tagLen = \case
  (Tag8 n _) -> n
  (TagBool _) -> 1
  TagByteFiller -> 8
  (TagWord64 _) -> 11 + 64
  (TagBytes bs) -> 8 * B.length bs
  (TagLazyBytes bs) -> 8 * (fromIntegral $ L.length bs)
