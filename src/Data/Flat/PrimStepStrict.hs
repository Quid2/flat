module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes) where

import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes
import           Data.Monoid

-- Strict encoder, calculate max length of encoding then alloc single buffer and encode unsafely.
-- Faster than Flat on anything but lN3
type Encoding = Step

instance Monoid Step where
  {-# INLINE mempty #-}
  mempty = Step 0 return

  {-# INLINE mappend #-}
  mappend (Step n f) (Step n2 g) = Step (n+n2) (f >=> g)

bitEncoder :: Encoding -> L.ByteString
bitEncoder (Step numBits op) = L.fromStrict $ bitEncoderStrict numBits op

bitEncoderL (Step numBits op) = bitEncoderLazy (numBits `div` 8 +1) (\(E fp s) op -> op s >>= done) op

