module Data.Flat.Encoding(eFiller,eBits,eBool,eFalse,eTrue,eWord8,eBitList,eUnsigned,eBytes,eLazyBytes,showEncoding) where

import Data.Flat.Types
import Data.Foldable(toList)

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
