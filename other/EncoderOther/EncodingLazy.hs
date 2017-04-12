module Data.Flat.Encoding(eFiller,eBits,eBool,eFalse,eTrue,eWord8,eWord32,eWord64,eBitList,eUnsigned,eBytes,eLazyBytes,showEncoding) where

import Data.Flat.TypesEncoding
import Data.Foldable(toList)
import Data.Shift
import Data.Word

showEncoding :: Encoding -> String
showEncoding = show . reverse . toList

-- |Encodings

{-# INLINE eFiller #-}
eFiller :: Encoding
eFiller = Leaf TagByteFiller

{-# INLINE eBits #-}
eBits :: Int -> Word8 -> Encoding
eBits numBits code = Leaf (Tag8 numBits code)

{-# INLINE eWord8 #-}
eWord8 :: Word8 -> Encoding
eWord8 = eBits 8

{-# INLINE eWord32 #-}
eWord32 :: Word32 -> Encoding
eWord32 w = Tag8 8 (fromIntegral $ w `shiftr_w32` 24) <|
            Tag8 8 (fromIntegral $ w `shiftr_w32` 16) <|
            Tag8 8 (fromIntegral $ w `shiftr_w32`  8) <|
            Tag8 8 (fromIntegral w) <|
            Empty

{-# INLINE eWord64 #-}
eWord64 :: Word64 -> Encoding
eWord64 w =
  Tag8 8 (fromIntegral $ w `shiftr_w64` 56) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 48) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 40) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 32) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 24) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 16) <|
  Tag8 8 (fromIntegral $ w `shiftr_w64` 8) <|
  Tag8 8 (fromIntegral w) <|
  Empty

{-# INLINE eFalse #-}
eFalse :: Encoding
eFalse = Leaf (TagBool False) -- eBits 1 0

{-# INLINE eTrue #-}
eTrue :: Encoding
eTrue = Leaf (TagBool True) -- eBits 1 1

{-# INLINE eBool #-}
eBool = Leaf . TagBool

-- eBitList :: (Monoid (s Op), Sequence s) => [s Op] -> s Op
eBitList l = mconcat (map (TagBool True <|) l) |> TagBool False

{-# INLINE eUnsigned #-}
eUnsigned :: Integral n => n -> Encoding
eUnsigned = Leaf . TagWord64 . fromIntegral

{-# INLINE eBytes #-}
--eBytes bs = Node (Leaf TagByteFiller) (Leaf $ TagBytes bs)
eBytes = Leaf . TagBytes

{-# INLINE eLazyBytes #-}
-- eLazyBytes bs =  Node (Leaf TagByteFiller) (Leaf $ TagLazyBytes bs)
eLazyBytes = Leaf . TagLazyBytes
