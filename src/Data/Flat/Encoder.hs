{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction ,ScopedTypeVariables #-}
module Data.Flat.Encoder (
    Encoding,
    (<>),
    NumBits,
    numBlks,
    arrayBits,
    encodersS,
    mempty,
    encoderStrict,
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
    eUTF16,
    sizeWord,
    eLazyBytes,
    eShortBytes,
    eInt,
    eInt8,
    eInt16,
    eInt32,
    eInt64,
    eWord,
    eChar,
    --eString,
    --sString,
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
    sFiller,
    sBool,
    ) where

-- import           Control.Monad
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes      hiding (eLazyBytes, eShortBytes, sBool,
                                       sBytes, sChar, sDouble, sFiller, sFloat,
                                       sInt, sInt16, sInt32, sInt64, sInt8,
                                       sInteger, sLazyBytes, sNatural,
                                       sShortBytes, sUTF16, sWord, sWord16,
                                       sWord32, sWord64, sWord8)
import qualified Data.Flat.Pokes      as P
-- -- -- -- import           Data.Flat.Size
import           Data.Foldable
import           Data.Monoid
-- -- -- -- import qualified Data.Sequences as O
-- -- -- import           Data.MonoTraversable

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

encoderStrict :: Int -> Encoding -> L.ByteString
encoderStrict numBits (Writer op) = L.fromStrict $ bitEncoderStrict numBits op

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
-- eArray :: Eq t => (t -> Writer -> [t] -> Writer
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

{-# INLINE eChar #-}
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


-- Store like Sizes
-- actual sizes
-- sChar = VarSize P.sCharV
-- sWord = VarSize P.sWordV
-- sInt = VarSize P.sIntV
-- sWord64 = VarSize P.sWord64V
-- sWord32 = VarSize P.sWord32V
-- sWord16 = VarSize P.sWord16V
-- sInt64 = VarSize P.sInt64V
-- sInt32 = VarSize P.sInt32V
-- sInt16 = VarSize P.sInt16V

-- -- Fixed sizes
-- sChar = ConstSize P.sChar
-- sInt64 = ConstSize P.sInt64
-- sInt32 = ConstSize P.sInt32
-- sInt16 = ConstSize P.sInt16
-- sInt8 = ConstSize P.sInt8
-- sInt = ConstSize P.sInt
-- sWord64 = ConstSize P.sWord64
-- sWord32 = ConstSize P.sWord32
-- sWord16 = ConstSize P.sWord16
-- sWord8 = ConstSize P.sWord8
-- sWord = ConstSize P.sWord

-- sBytes = VarSize P.sBytes
-- sLazyBytes = VarSize P.sLazyBytes
-- sShortBytes = VarSize P.sShortBytes
-- sNatural = VarSize P.sNatural
-- sInteger = VarSize P.sInteger
-- sUTF16 = VarSize P.sUTF16

-- sInt8 = ConstSize P.sInt8
-- sWord8 = ConstSize P.sWord8
-- sFloat = ConstSize P.sFloat
-- sDouble = ConstSize P.sDouble
-- sFiller = ConstSize P.sFiller
-- sBool = ConstSize P.sBool

-- Simple sizes
{-# INLINE vsize #-}
vsize :: (t -> NumBits) -> t -> NumBits -> NumBits
vsize f t n = f t + n

{-# INLINE csize #-}
csize :: NumBits -> t -> NumBits -> NumBits
csize !n _ !s = n+s

sChar = vsize P.sCharV
sInt64 = vsize P.sInt64V
sInt32 = vsize P.sInt32V
sInt16 = vsize P.sInt16V
sInt = vsize P.sIntV
sWord64 = vsize P.sWord64V
sWord32 = vsize P.sWord32V
sWord16 = vsize P.sWord16V
sWord = vsize P.sWordV

sBytes = vsize P.sBytes
sLazyBytes = vsize P.sLazyBytes
sShortBytes = vsize P.sShortBytes
sNatural = vsize P.sNatural
sInteger = vsize P.sInteger
sUTF16 = vsize P.sUTF16

sInt8 = csize P.sInt8
sWord8 = csize P.sWord8
sFloat = csize P.sFloat
sDouble = csize P.sDouble
sFiller = csize P.sFiller
sBool = csize P.sBool
