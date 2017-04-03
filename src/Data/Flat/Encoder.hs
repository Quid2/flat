{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Data.Flat.Encoder (
    Encoding,
    (<>),
    NumBits,
    --numBlks,
    --arrayBits,
    encodersS,
    mempty,
    encoderStrict,
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
    eUTF16,
    eLazyBytes,
    eShortBytes,
    eInt,
    eInt8,
    eInt16,
    eInt32,
    eInt64,
    eWord,
    eChar,
    eArray,
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
    sBytes,
    sLazyBytes,
    sShortBytes,
    sUTF16,
    sFillerMax,
    sBool
    --,sUTF8,eUTF8
    ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Memory
import           Data.Flat.Poke
import qualified Data.Flat.Size       as S
import           Data.Flat.Types
import           Data.Foldable
import           Data.Monoid

-- |Strict encoder
encoderStrict :: NumBits -> Encoding -> B.ByteString
encoderStrict numBits (Writer op) =
  let bufSize = S.bitsToBytes numBits
  in fst $ unsafeCreateUptoN' bufSize $
        \ptr -> do
          (S ptr' 0 0) <- op (S ptr 0 0)
          return (ptr' `minusPtr` ptr,())

-- Strict encoder, calculate max length of encoding then alloc single buffer and encode unsafely.
type Encoding = Writer

-- newtype vs data, way faster for nested structures, slower for some fixed size ones
newtype Writer = Writer {runWriter::S-> IO S}
instance Show Writer where show _ = "Writer"

instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  -- mappend (Writer f) (Writer g) = Writer (f >=> g)
  mappend (Writer f) (Writer g) = Writer m
    where m !s@(S !_ !_ !_) = do
            !s1 <- f s
            g s1

  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

-- The special cases considerably speed up compilation time, at least for 7.10.3
{-# RULES
"encodersSN" forall h t. encodersS (h:t) = h `mappend` encodersS t
"encodersS0" encodersS [] = mempty
 #-}

{-# NOINLINE encodersS #-}
-- PROB: GHC 8.02 won't always apply the rules leading to poor execution times (e.g. with lists)
encodersS :: [Writer] -> Writer
-- without the explicit parameter the rules won't fire
encodersS ws =  foldl' mappend mempty ws
-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]

-- encodersS ws = mconcat ws -- BAD: rules won't fire (gets inlined before rules are applied?)

-- {-# INLINE eList #-}
-- eList ws e = Writer $ \s ->
--    eWord8 0 s >>= go ws 0 s
--   where
--     go [] s =
--     go (x:xs) n ns s = e x

-- {-# INLINE eString #-}
-- eString :: String -> Writer
-- eString = eArray eChar

-- sString l n = n + length l * 32 -- BAD

{-# INLINE eArray #-}
-- |Encode as Array
eArray :: (t -> Writer) -> [t] -> Writer
eArray _ [] = eWord8 0
eArray f ws = Writer $ go ws
  where
    go l s = do
      s' <- eWord8F 0 s
      (n,s'',l) <- gol l 0 s'
      _ <- eWord8F n s
      if length l == 0 -- l==[]
        then eWord8F 0 s''
        else go l s''

    gol []       !n !s  = return (n,s,[])
    gol l@(x:xs) !n !s | n == 255 = return (255,s,l)
                       | otherwise = runWriter (f x) s >>= gol xs (n+1)

-- Encoding primitives
{-# INLINE eChar #-}
--{-# INLINE eUTF8 #-}
{-# INLINE eUTF16 #-}
{-# INLINE eNatural #-}
{-# INLINE eFloat #-}
{-# INLINE eDouble #-}
{-# INLINE eInteger #-}
{-# INLINE eInt64 #-}
{-# INLINE eInt32 #-}
{-# INLINE eInt16 #-}
{-# INLINE eInt8 #-}
{-# INLINE eInt #-}
{-# INLINE eWord64 #-}
{-# INLINE eWord32 #-}
{-# INLINE eWord16 #-}
{-# INLINE eWord8 #-}
{-# INLINE eWord #-}
{-# INLINE eBits #-}
{-# INLINE eFiller #-}
{-# INLINE eBool #-}
{-# INLINE eTrue #-}
{-# INLINE eFalse #-}

eChar :: Char -> Writer
eChar = Writer . eCharF
eUTF16 :: Text -> Writer
eUTF16 = Writer . eUTF16F
--eUTF8 :: Text -> Writer
--eUTF8 = Writer . eUTF8F
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

-- Size Primitives

-- Variable size
{-# INLINE vsize #-}
vsize :: (t -> NumBits) -> t -> NumBits -> NumBits
vsize !f !t !n = f t + n

-- Constant size
{-# INLINE csize #-}
csize :: NumBits -> t -> NumBits -> NumBits
csize !n _ !s = n+s

sChar :: Size Char
sChar = vsize S.sChar

sInt64 :: Size Int64
sInt64 = vsize S.sInt64

sInt32 :: Size Int32
sInt32 = vsize S.sInt32

sInt16 :: Size Int16
sInt16 = vsize S.sInt16

sInt8 :: Size Int8
sInt8 = csize S.sInt8

sInt :: Size Int
sInt = vsize S.sInt

sWord64 :: Size Word64
sWord64 = vsize S.sWord64

sWord32 :: Size Word32
sWord32 = vsize S.sWord32

sWord16 :: Size Word16
sWord16 = vsize S.sWord16

sWord8 :: Size Word8
sWord8 = csize S.sWord8

sWord :: Size Word
sWord = vsize S.sWord

sFloat :: Size Float
sFloat = csize S.sFloat

sDouble :: Size Double
sDouble = csize S.sDouble

sBytes :: Size B.ByteString
sBytes = vsize S.sBytes

sLazyBytes :: Size L.ByteString
sLazyBytes = vsize S.sLazyBytes

sShortBytes :: Size ShortByteString
sShortBytes = vsize S.sShortBytes

sNatural :: Size Natural
sNatural = vsize S.sNatural

sInteger :: Size Integer
sInteger = vsize S.sInteger
--sUTF8 = vsize S.sUTF8

sUTF16 :: Size Text
sUTF16 = vsize S.sUTF16


sFillerMax :: Size a
sFillerMax = csize S.sFillerMax

sBool :: Size Bool
sBool = csize S.sBool
