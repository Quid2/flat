module Data.ZigZag where

import Data.Word
import Data.Int
import Data.Bits

{-# INLINE zzEncode8 #-}
zzEncode8 :: Int8 -> Word8
zzEncode8 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 7))

{-# INLINE zzEncode16 #-}
zzEncode16 :: Int16 -> Word16
zzEncode16 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 15))

{-# INLINE zzEncode32 #-}
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))

{-# INLINE zzEncode64 #-}
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))

{-# INLINE zzDecode8 #-}
zzDecode8 :: Word8 -> Int8
zzDecode8 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecode16 #-}
zzDecode16 :: Word16 -> Int16
zzDecode16 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecode32 #-}
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecode64 #-}
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecodeInteger #-}
zzDecodeInteger :: Integer -> Integer
zzDecodeInteger w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzDecode #-}
zzDecode :: (Bits a1, Bits a, Num a, Integral a1) => a1 -> a
zzDecode w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

{-# INLINE zzEncodeInteger #-}
zzEncodeInteger :: Integer -> Integer
zzEncodeInteger x | x>=0      = x `shiftL` 1
                  | otherwise = negate (x `shiftL` 1) - 1
