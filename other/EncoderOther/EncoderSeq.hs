{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
-- |Binary Encoder
module Data.Flat.Encoder(bitEncoder) where

import           Data.Bits
import qualified Data.ByteString                  as B
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Builder.Internal
import qualified Data.ByteString.Lazy             as L
import           Data.Flat.Types
import           Foreign.Ptr

bitEncoder :: Encoding -> ByteString
-- bitEncoder = toLazyByteString . toBitBuilder
bitEncoder = toLazyByteStringWith (untrimmedStrategy smallChunkSize defaultChunkSize) L.empty . toBitBuilder

-- Note: last byte is 0-padded (not an issue if encoded value is terminated with a Filler)
toBitBuilder :: Encoding -> Builder
toBitBuilder =
  \e -> builder (step e 0 0)
  where
    step e1 w o k (BufferRange op0 ope0) = {-# SCC "go" #-} go e1 w o op0
      where
        go e w o op
          | op `plusPtr` bound <= ope0 = -- This check might be avoided when we do not write additional bytes
            goUnsafe e w o op
          | otherwise = {-# SCC "OutTag8_bufferFull"#-} return $ bufferFull bytesToAdd op (step e w o k)
        goUnsafe
          e   -- Tags sequence
          !w  -- Byte being filled
          !o  -- Number of used bits (invariant 0<=o<=7, if o==8 byte is saved to memory)
          !op -- Pointer to next available byte in buffer
          =
              case viewl e of
                (n :< e') -> case n of
                   TagBool b -> {-# SCC "TagBool" #-}
                        let w' = if b then setBit w (7-o) else w
                        in if o == 7
                              then runB bword8 w' op >>= go e' 0 0
                              else goUnsafe e' w' (o+1) op

                   -- Tag8 8 t | o == 0 -> runB bword8 t op >>= go e' w o
                   -- Tag8 n t | o == 0 -> go e' (t `shiftL` (8-n)) n op
                   Tag8 n t -> {-# SCC "OutTag8" #-}
                        let o' = o + n
                            f = 8 - o'  -- remaining free bits
                        in if | f > 0  -> {-# SCC "OutTag8_f>0" #-} goUnsafe e' (w .|. (t `shiftL` f)) o' op
                              -- | f == 0 -> {-# SCC "OutTag8_f=0" #-} runB bword8 (w .|. (t `shiftL` f)) op >>= go e' 0 0
                              | f == 0 -> {-# SCC "OutTag8_f=0" #-} runB bword8 (w .|. t) op >>= go e' 0 0
                              | otherwise -> {-# SCC "OutTag8_f<0" #-}
                                       let o' = -f
                                           b' = w .|. (t `shiftR` o')
                                           w' = t `shiftL` (8 - o')
                                       in {-# SCC "OutTag8_bword8"#-} runB bword8 b' op >>= go e' w' o'

                   -- Encode as data NEList = Elem Word7 | Cons Word7 List
                   TagWord64 t | o == 0 -> tag64 t op >>= go e' w o
                         where tag64 t op =
                                  let l  = (fromIntegral t) .&. 0x7F
                                      t' = t `shiftR` 7
                                  in if t' == 0
                                     then runB bword8 l op
                                     else runB bword8 (l .|. 0x80) op >>= tag64 t'

                   TagWord64 t -> let l  = (fromIntegral t) .&. 0x7F
                                      t' = t `shiftR` 7
                                  in if t' == 0
                                     then go (Tag8 8 l <| e') w o op
                                     else go (Tag8 8 (l .|. 0x80) <| TagWord64 t' <| e') w o op

                   TagByteFiller -> runB bword8 (w .|. 1) op >>= go e' 0 0

                   TagBytes bs | o==0 ->  runBuilderWith (bsMP bs) (step e' 0 0 k) (BufferRange op ope0)
                               | otherwise -> error "Encoder doesn't support unaligned bytestrings."

                   TagLazyBytes bs | o==0 -> runBuilderWith (bsLazyMP bs) (step e' 0 0 k) (BufferRange op ope0)
                                   | otherwise -> error "Encoder doesn't support unaligned bytestrings."

                   -- Nop -> go e' w o op

                EmptyL -> do
                       op' <-if o==0
                             then return op
                             else runB bword8 w op
                       k (BufferRange op' ope0)


        -- The maximum size in bytes of the fixed-size encodings
        bound :: Int
        bound = 11

        -- How many additional bytes to allocate when the buffer is empty
        bytesToAdd :: Int
        bytesToAdd = bound

bsMP bs | B.length bs == 0 = word8 0
        | otherwise = let (h,t) = B.splitAt 255 bs
                      in word8 (fromIntegral $ B.length h) <> byteString h <> bsMP t

bsLazyMP bs | L.length bs == 0 = word8 0
            | otherwise = let (h,t) = L.splitAt 255 bs
                          in word8 (fromIntegral $ L.length h) <> lazyByteString h <> bsLazyMP t

