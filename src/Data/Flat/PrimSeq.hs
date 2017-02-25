module Data.Flat.Prim(Encoding,(<>),(<+>),mempty,bitEncoder,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where
import Data.Flat.Types
import Data.Flat.EncoderLazy
import Data.Flat.Encoding
import Data.Word

{-# INLINE (<+>) #-}
(<+>) :: Encoding -> Encoding -> Encoding
(<+>) = (<>)

eUTF16 = error "not there"
eShortBytes = error "not there"
eUnsigned64 :: Word64 -> Encoding
eUnsigned64 = eUnsigned
eUnsigned32 :: Word32 -> Encoding
eUnsigned32 = eUnsigned
eUnsigned16 :: Word16 -> Encoding
eUnsigned16 = eUnsigned
eWord32BE = eWord32
eWord64BE = eWord64

