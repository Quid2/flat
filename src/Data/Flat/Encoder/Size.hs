{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- |Primitives to calculate the encoding size of a value
module Data.Flat.Encoder.Size where

import           Data.Bits
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Char
import           Data.Flat.Encoder.Prim         (w7l)
import           Data.Flat.Encoder.Types
import           Data.Flat.Types
import qualified Data.Text                      as T
#ifndef ghcjs_HOST_OS
import qualified Data.Text.Internal             as TI
#endif
import           Data.ZigZag
#include "MachDeps.h"
-- A filler can take anything from 1 to 8 bits
sFillerMax :: NumBits
sFillerMax = 8

sBool :: NumBits
sBool = 1

sWord8 :: NumBits
sWord8 = 8

sInt8 :: NumBits
sInt8 = 8

sFloat :: NumBits
sFloat = 32

sDouble :: NumBits
sDouble = 64

{-# INLINE sChar #-}
sChar :: Char -> NumBits
sChar = sWord32 . fromIntegral . ord

sCharMax :: NumBits
sCharMax = 24

{-# INLINE sWord #-}
sWord :: Word -> NumBits
{-# INLINE sInt #-}
sInt :: Int -> NumBits
#if WORD_SIZE_IN_BITS == 64
sWord = sWord64 . fromIntegral

sInt = sInt64 . fromIntegral
#elif WORD_SIZE_IN_BITS == 32
sWord = sWord32 . fromIntegral

sInt = sInt32 . fromIntegral
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif
-- TODO: optimize ints sizes
{-# INLINE sInt16 #-}
sInt16 :: Int16 -> NumBits
sInt16 = sWord16 . zzEncode

{-# INLINE sInt32 #-}
sInt32 :: Int32 -> NumBits
sInt32 = sWord32 . zzEncode

{-# INLINE sInt64 #-}
sInt64 :: Int64 -> NumBits
sInt64 = sWord64 . zzEncode

{-# INLINE sWord16 #-}
sWord16 :: Word16 -> NumBits
sWord16 w
  | w < 128 = 8
  | w < 16384 = 16
  | otherwise = 24

{-# INLINE sWord32 #-}
sWord32 :: Word32 -> NumBits
sWord32 w
  | w < 128 = 8
  | w < 16384 = 16
  | w < 2097152 = 24
  | w < 268435456 = 32
  | otherwise = 40

{-# INLINE sWord64 #-}
sWord64 :: Word64 -> NumBits
sWord64 w
  | w < 128 = 8
  | w < 16384 = 16
  | w < 2097152 = 24
  | w < 268435456 = 32
  | w < 34359738368 = 40
  | w < 4398046511104 = 48
  | w < 562949953421312 = 56
  | w < 72057594037927936 = 64
  | w < 9223372036854775808 = 72
  | otherwise = 80

{-# INLINE sInteger #-}
sInteger :: Integer -> NumBits
sInteger = sIntegral . zzEncodeInteger

{-# INLINE sNatural #-}
sNatural :: Natural -> NumBits
sNatural = sIntegral . toInteger

-- BAD: duplication of work with encoding
{-# INLINE sIntegral #-}
sIntegral :: (Bits t, Integral t) => t -> Int
sIntegral t =
  let vs = w7l t
   in length vs * 8

--sUTF8 :: T.Text -> NumBits
--sUTF8 t = fold
-- Wildly pessimistic but fast
{-# INLINE sUTF8Max #-}
sUTF8Max :: Text -> NumBits
sUTF8Max = blobBits . (4 *) . T.length
#ifndef ghcjs_HOST_OS
{-# INLINE sUTF16 #-}
sUTF16 :: T.Text -> NumBits
sUTF16 = blobBits . textBytes
#endif
{-# INLINE sBytes #-}
sBytes :: B.ByteString -> NumBits
sBytes = blobBits . B.length

{-# INLINE sLazyBytes #-}
sLazyBytes :: L.ByteString -> NumBits
sLazyBytes bs = 16 + L.foldrChunks (\b l -> blkBitsBS b + l) 0 bs

{-# INLINE sShortBytes #-}
sShortBytes :: SBS.ShortByteString -> NumBits
sShortBytes = blobBits . SBS.length

#ifndef ghcjs_HOST_OS
-- We are not interested in the number of unicode chars (returned by T.length, an O(n) operation)
-- just the number of bytes
-- > T.length (T.pack "\x1F600")
-- 1
-- > textBytes (T.pack "\x1F600")
-- 4
{-# INLINE textBytes #-}
textBytes :: T.Text -> Int
textBytes !(TI.Text _ _ w16Len) = w16Len * 2
#endif

{-# INLINE bitsToBytes #-}
bitsToBytes :: Int -> Int
bitsToBytes = numBlks 8

{-# INLINE numBlks #-}
numBlks :: Integral t => t -> t -> t
numBlks blkSize bits =
  let (d, m) = bits `divMod` blkSize
   in d +
      (if m == 0
         then 0
         else 1)

{-# INLINE arrayBits #-}
arrayBits :: Int -> NumBits
arrayBits = (8 *) . arrayChunks

{-# INLINE arrayChunks #-}
arrayChunks :: Int -> NumBits
arrayChunks = (1 +) . numBlks 255

{-# INLINE blobBits #-}
blobBits :: Int -> NumBits
blobBits numBytes =
  16 -- initial filler + final 0
   +
  blksBits numBytes

{-# INLINE blkBitsBS #-}
blkBitsBS :: B.ByteString -> NumBits
blkBitsBS = blksBits . B.length

{-# INLINE blksBits #-}
blksBits :: Int -> NumBits
blksBits numBytes = 8 * (numBytes + numBlks 255 numBytes)
