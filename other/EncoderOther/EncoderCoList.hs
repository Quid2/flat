{-# LANGUAGE BangPatterns ,NoMonomorphismRestriction #-}
module Data.Flat.Encoder (
    Encoding,
    (<>),
    NumBits,
    numBlks,
    encodersS,
    mempty,
    bitEncoder,
    eBitsS,
    eTrueF,
    eFalseF,
    eFloat,
    eDouble,
    eInteger,
    eNatural,
    eWord16,
    eWord32,
    eWord64,
    eWord8,
    eBits,
    eFiller,
    eBool,
    eTrue,
    eFalse,
    eBytes,
    sBytes,
    eLazyBytes,
    sLazyBytes,
    eShortBytes,
    sShortBytes,
    eUTF16,
    sizeWord,
    sUTF16,
    eInt,
    eInt8,
    eInt16,
    eInt32,
    eInt64,
    eWord,
    sWord,
    sWord8,
    sWord16,
    sWord32,
    sWord64,
    sInt,
    sInt8,
    sInt16,
    sInt32,
    sInt64,
    sNatural,
    sInteger,
    sFloat,
    sDouble,
    sChar,
    eChar
    ) where

import           Control.Monad
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes
import           Data.Foldable
import           Data.Monoid
import Control.Exception

-- Lazy encoder
type Encoding = Writer

-- newtype vs data, way faster for nested structures, slower for some fixed size ones
newtype Writer = Writer {runWriter :: [Writer] -> E -> IO E}
instance Show Writer where show (Writer _) = "Writer"

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderLazy 32 encoder

encoder :: E -> Encoding -> IO (Signal Encoding)

encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (encoders (traceShow (unwords ["encoder",show ws]) ws)))

data NotEnoughSpaceException = NotEnoughSpaceException S Int [[Writer]] deriving Show
instance Exception NotEnoughSpaceException

{-# INLINE writer #-}
writer :: Step -> Writer
writer (Step n f) = me
  where
    me = Writer prim
    prim k e@(E p s) | n <= availBits e = f s >>= return . E p
                     | otherwise = throw (NotEnoughSpaceException s n [[me]])


{-# RULES
-- "encodersSN" forall h t. encodersS (h:t) = Writer $ \k-> runWriter h (t++k) >=> encodersS t
"encodersS3" forall a1 a2 a3. encodersS [a1,a2,a3] = Writer $ \k -> runWriter a1 (a2:a3:k) >=> runWriter a2 (a3:k) >=> runWriter a3 k
"encodersS2" forall a1 a2. encodersS [a1,a2] = Writer $ \k -> runWriter a1 (a2:k) >=> runWriter a2 k
"encodersS1" forall a. encodersS [a] = a
"encodersS0" encodersS [] = mempty
 #-}

{-# NOINLINE encodersS #-}
encodersS :: [Writer] -> Writer
-- without the explicit parameter the rules won't fire
encodersS ws = Writer $ encodersS_ ws
encodersS_ k (h:t) = runWriter h (t++k) >=> encodersS_ k t
encodersS_ k [] = return
-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]

class Checked c where
  checked :: c -> Writer

eBytes :: B.ByteString -> Writer
eBytes = writer . eBytesS
eLazyBytes :: L.ByteString -> Writer
eLazyBytes = writer . eLazyBytesS
eShortBytes :: ShortByteString -> Writer
eShortBytes = writer . eShortBytesS
eNatural :: Natural -> Writer
eNatural = writer . eNaturalS
eInteger :: Integer -> Writer
eInteger = writer . eIntegerS

eChar :: Char -> Writer
eChar = writer . eCharS
eUTF16 :: Text -> Writer
eUTF16 = writer . eUTF16S
eFloat :: Float -> Writer
eFloat = writer . eFloatS
eDouble :: Double -> Writer
eDouble = writer . eDoubleS
eInt64 :: Int64 -> Writer
eInt64 = writer . eInt64S
eInt32 :: Int32 -> Writer
eInt32 = writer . eInt32S
eInt16 :: Int16 -> Writer
eInt16 = writer . eInt16S
eInt8 :: Int8 -> Writer
eInt8 = writer . eInt8S
eInt :: Int -> Writer
eInt = writer . eIntS
eWord64 :: Word64 -> Writer
eWord64 = writer . eWord64S
eWord32 :: Word32 -> Writer
eWord32 = writer . eWord32S
eWord16 :: Word16 -> Writer
eWord16 = writer . eWord16S
eWord8 :: Word8 -> Writer
eWord8 = writer . eWord8S
eWord :: Word -> Writer
eWord = writer . eWordS
eBits :: NumBits -> Word8 -> Writer
eBits n f = Writer $ eBitsS n f
eFiller :: Writer
eFiller = writer eFillerS
eBool :: Bool -> Writer
eBool = writer . eBoolS
eTrue :: Writer
eTrue = writer eTrueS
eFalse :: Writer
eFalse = writer eFalseS
