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
import           Data.Monoid hiding ((<>))
import           Data.Semigroup

-- Strict encoder, calculate max length of encoding then alloc single buffer and encode unsafely.
type Encoding = Writer

-- newtype vs data, way faster for nested structures, slower for some fixed size ones
newtype Writer = Writer {runWriter::S-> IO S}
instance Show Writer where show _ = "Writer"

instance Semigroup Writer where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  mappend (Writer f) (Writer g) = Writer (f >=> g)

  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

bitEncoder :: Int -> Encoding -> L.ByteString
bitEncoder numBits (Writer op) = L.fromStrict $ bitEncoderStrict numBits op

{-# RULES
"encodersSN" forall h t. encodersS (h:t) = h `mappend` encodersS t
"encodersS1" forall a. encodersS [a] = a
"encodersS0" encodersS [] = mempty
 #-}

{-# NOINLINE encodersS #-}
encodersS :: [Writer] -> Writer
-- without the explicit parameter the rules won't fire
encodersS ws =  foldl' mappend mempty ws
-- encodersS ws = mconcat ws -- BAD: rules won't fire (gets inlined before rules are applied?)
-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]

eChar :: Char -> Writer
eChar = Writer . eCharF
eUTF16 :: Text -> Writer
eUTF16 = Writer . eUTF16F
eBytes :: B.ByteString -> Writer
eBytes = Writer . eBytesF
eLazyBytes :: L.ByteString -> Writer
eLazyBytes = Writer . eLazyBytesF
eShortBytes :: ShortByteString -> Writer
eShortBytes = Writer. eShortBytesF
eNatural :: Natural -> Writer
eNatural = Writer. eNaturalF
eFloat :: Float -> Writer
eFloat = Writer . eFloatF
eDouble :: Double -> Writer
eDouble = Writer . eDoubleF
eInteger :: Integer -> Writer
eInteger = Writer. eIntegerF
eInt64 :: Int64 -> Writer
eInt64 = Writer. eInt64F
eInt32 :: Int32 -> Writer
eInt32 = Writer. eInt32F
eInt16 :: Int16 -> Writer
eInt16 = Writer. eInt16F
eInt8 :: Int8 -> Writer
eInt8 = Writer . eInt8F
eInt :: Int -> Writer
eInt = Writer . eIntF
eWord64 :: Word64 -> Writer
eWord64 = Writer. eWord64F
eWord32 :: Word32 -> Writer
eWord32 = Writer. eWord32F
eWord16 :: Word16 -> Writer
eWord16 = Writer. eWord16F
eWord8 :: Word8 -> Writer
eWord8 = Writer . eWord8F
eWord :: Word -> Writer
eWord = Writer . eWordF
eBits :: NumBits -> Word8 -> Writer
eBits n f = Writer $ eBitsF n f
eFiller :: Writer
eFiller = Writer eFillerF
eBool :: Bool -> Writer
eBool = Writer . eBoolF
eTrue :: Writer
eTrue = Writer eTrueF
eFalse :: Writer
eFalse = Writer eFalseF
