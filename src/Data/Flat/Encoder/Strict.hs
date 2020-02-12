{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |Strict encoder
module Data.Flat.Encoder.Strict where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Flat.Encoder.Prim
import qualified Data.Flat.Encoder.Size  as S
import           Data.Flat.Encoder.Types
import           Data.Flat.Memory
import           Data.Flat.Types
import           Data.Foldable

-- import           Data.Semigroup
-- import           Data.Semigroup          (Semigroup (..))

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup          (Semigroup (..))
#endif

#ifdef ETA_VERSION
-- import Data.Function(trampoline)
import           GHC.IO                  (trampolineIO)
trampolineEncoding :: Encoding -> Encoding
trampolineEncoding (Encoding op) = Encoding (\s -> trampolineIO (op s))
#else

-- trampolineIO = id
#endif

-- |Strict encoder
strictEncoder :: NumBits -> Encoding -> B.ByteString
strictEncoder numBits (Encoding op) =
  let bufSize = S.bitsToBytes numBits
   in fst $
      unsafeCreateUptoN' bufSize $ \ptr -> do
        (S ptr' 0 0) <- op (S ptr 0 0)
        return (ptr' `minusPtr` ptr, ())

newtype Encoding =
  Encoding
    { run :: Prim
    }

instance Show Encoding where
  show _ = "Encoding"

instance Semigroup Encoding where
  {-# INLINE (<>) #-}
  (<>) = mappend

instance Monoid Encoding where
  {-# INLINE mempty #-}
  mempty = Encoding return
  {-# INLINE mappend #-}
  -- mappend (Encoding f) (Encoding g) = Encoding (f >=> g)
  mappend (Encoding f) (Encoding g) = Encoding m
    where
      m s@(S !_ !_ !_) = do
        !s1 <- f s
        g s1
  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

-- PROB: GHC 8.02 won't always apply the rules leading to poor execution times (e.g. with lists)
{-# RULES
"encodersSN" forall h t . encodersS (h : t) =
             h `mappend` encodersS t
"encodersS0" encodersS [] = mempty
 #-}

{-# NOINLINE encodersS #-}
encodersS :: [Encoding] -> Encoding
-- without the explicit parameter the rules won't fire
encodersS ws = foldl' mappend mempty ws

-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]
{-# INLINE encodeListWith #-}
-- |Encode as a List
encodeListWith :: (t -> Encoding) -> [t] -> Encoding
encodeListWith enc = go
  where
    go []     = eFalse
    go (x:xs) = eTrue <> enc x <> go xs
 
-- {-# INLINE encodeList #-}
-- encodeList :: (Foldable t, Flat a) => t a -> Encoding
-- encodeList l = F.foldl' (\acc a -> acc <> eTrue <> encode a) mempty l <> eFalse
-- {-# INLINE encodeList2 #-}
-- encodeList2 :: (Foldable t, Flat a) => t a -> Encoding
-- encodeList2 l = foldr (\a acc -> eTrue <> encode a <> acc) mempty l <> eFalse
{-# INLINE encodeArrayWith #-}
-- |Encode as Array
encodeArrayWith :: (t -> Encoding) -> [t] -> Encoding
encodeArrayWith _ [] = eWord8 0
encodeArrayWith f ws = Encoding $ go ws
  where
    go l s = do
      s' <- eWord8F 0 s
      (n, s'', l) <- gol l 0 s'
      _ <- eWord8F n s
      if null l
        then eWord8F 0 s''
        else go l s''
    gol [] !n !s = return (n, s, [])
    gol l@(x:xs) !n !s
      | n == 255 = return (255, s, l)
      | otherwise = run (f x) s >>= gol xs (n + 1)

-- Encoding primitives
{-# INLINE eChar #-}
{-# INLINE eUTF8 #-}
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
eChar :: Char -> Encoding
eChar = Encoding . eCharF
#if! defined(ghcjs_HOST_OS) && ! defined (ETA_VERSION)
{-# INLINE eUTF16 #-}
eUTF16 :: Text -> Encoding
eUTF16 = Encoding . eUTF16F
#endif
eUTF8 :: Text -> Encoding
eUTF8 = Encoding . eUTF8F

eBytes :: B.ByteString -> Encoding
eBytes = Encoding . eBytesF

eLazyBytes :: L.ByteString -> Encoding
eLazyBytes = Encoding . eLazyBytesF

eShortBytes :: ShortByteString -> Encoding
eShortBytes = Encoding . eShortBytesF

eNatural :: Natural -> Encoding
eNatural = Encoding . eNaturalF

eFloat :: Float -> Encoding
eFloat = Encoding . eFloatF

eDouble :: Double -> Encoding
eDouble = Encoding . eDoubleF

eInteger :: Integer -> Encoding
eInteger = Encoding . eIntegerF

eInt64 :: Int64 -> Encoding
eInt64 = Encoding . eInt64F

eInt32 :: Int32 -> Encoding
eInt32 = Encoding . eInt32F

eInt16 :: Int16 -> Encoding
eInt16 = Encoding . eInt16F

eInt8 :: Int8 -> Encoding
eInt8 = Encoding . eInt8F

eInt :: Int -> Encoding
eInt = Encoding . eIntF

eWord64 :: Word64 -> Encoding
eWord64 = Encoding . eWord64F

eWord32 :: Word32 -> Encoding
eWord32 = Encoding . eWord32F

eWord16 :: Word16 -> Encoding
eWord16 = Encoding . eWord16F

eWord8 :: Word8 -> Encoding
eWord8 = Encoding . eWord8F

eWord :: Word -> Encoding
eWord = Encoding . eWordF

eBits16 :: NumBits -> Word16 -> Encoding
eBits16 n f = Encoding $ eBits16F n f

eBits :: NumBits -> Word8 -> Encoding
eBits n f = Encoding $ eBitsF n f

eFiller :: Encoding
eFiller = Encoding eFillerF

eBool :: Bool -> Encoding
eBool = Encoding . eBoolF

eTrue :: Encoding
eTrue = Encoding eTrueF

eFalse :: Encoding
eFalse = Encoding eFalseF

-- Size Primitives
-- Variable size
{-# INLINE vsize #-}
vsize :: (t -> NumBits) -> t -> NumBits -> NumBits
vsize !f !t !n = f t + n

-- Constant size
{-# INLINE csize #-}
csize :: NumBits -> t -> NumBits -> NumBits
csize !n _ !s = n + s

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

-- sUTF8 = vsize S.sUTF8
sUTF8Max :: Size Text
sUTF8Max = vsize S.sUTF8Max
#ifndef ghcjs_HOST_OS
sUTF16 :: Size Text
sUTF16 = vsize S.sUTF16
#endif
sFillerMax :: Size a
sFillerMax = csize S.sFillerMax

sBool :: Size Bool
sBool = csize S.sBool
