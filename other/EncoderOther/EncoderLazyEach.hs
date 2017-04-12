{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Flat.Encoder (
    Encoding,
    mempty,
    NumBits,
    numBlks,
    encodersS,
    encoderLazy,
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

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Pokes
-- import           Data.Foldable
-- import Data.Monoid
-- import           Debug.Trace
import           Prelude              hiding (mempty)
traceShow _ v = v

{-
Lazy encoder

At the beginning of every constructor, perform a minimum check assuming that are the costructors parameters are all fixed size primitives with max size = 80, so check for 80*numPars bits.

Fixed size primitives do not need to perform any check.

Variable size primitives, are passed.
-}

type Encoding = Writer

-- Writers to execute, after current one
type Todo = [Writer]

newtype Writer = Writer {runWriter :: WriterF}

type WriterF = Todo -> E -> IO E

instance Show Writer where show (Writer _) = "Writer"

encoderLazy :: Encoding -> L.ByteString
encoderLazy = bitEncoderLazy 8 encoder

encoder :: E -> Encoding -> IO (Signal Encoding)
encoder e w = catch (runWriter w [] e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (encodersS (traceShow (unwords ["neededBits",show neededBits,"encoder",show ws]) ws)))

data NotEnoughSpaceException = NotEnoughSpaceException S Int Todo deriving Show
instance Exception NotEnoughSpaceException

-- instance Monoid Writer where
--   {-# INLINE mempty #-}
--   mempty = Writer $ \_ e -> return e

--   {-# INLINE mappend #-}
--   mappend (Writer f) (Writer g) = Writer $ \ks e -> f ks e >>= g ks

--   {-# INLINE mconcat #-}
--   mconcat = foldl' mappend mempty

{-# INLINE mempty #-}
mempty :: Writer
mempty = Writer $ \_ e -> return e

{- # RULES
-- "encodersSN" forall h t. encodersS (h:t) = h `mappend` encodersS t
"encodersSN" forall ws. encodersS ws = Writer $ \ks -> encoders ks ws

 # -}

{-# RULES
"encodersS4" forall a1 a2 a3 a4. encodersS [a4,a3,a2,a1] = Writer $ \ks -> runWriter a4 (a3:a2:a1:ks) >=> runWriter a3 (a2:a1:ks) >=> runWriter a2 (a1:ks) >=> runWriter a1 ks
"encodersS3" forall a1 a2 a3. encodersS [a1,a2,a3] = Writer $ \ks -> runWriter a1 (a2:a3:ks) >=> runWriter a2 (a3:ks) >=> runWriter a3 ks
"encodersS2" forall a1 a2. encodersS [a1,a2] = Writer $ \ks -> runWriter a1 (a2:ks) >=> runWriter a2 ks
"encodersS1" forall a1. encodersS [a1] = a1
"encodersS0" encodersS [] = mempty
 #-}

{-# NOINLINE encodersS #-}
encodersS :: [Writer] -> Writer
-- without the explicit parameter the rules won't fire
-- encodersS ws =  foldl' mappend mempty ws
encodersS [] = mempty
encodersS [a] = a
encodersS ws = Writer $ \ks -> encoders ks ws
-- encodersS ws = Writer $ \ks -> checkSize (minLen ws) (ws++ks) >=> encoders ks ws
-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]

{-# INLINE encoders #-}
encoders :: Todo -> WriterF
encoders _ [] = return
encoders ks [w]   = runWriter w ks
encoders ks (h:t) = runWriter h (t++ks) >=> encoders ks t

{-# INLINE writerV #-}
-- Variable size writer
writerV :: Step -> Writer
writerV (Step neededBits f) = me
  where
    me = Writer prim
    prim ks e@(E p s) | neededBits <= availBits e = f s >>= return . E p
                      | otherwise = throw (NotEnoughSpaceException s neededBits (me:ks))

{-# INLINE writerF #-}
writerF :: Prim -> Writer
writerF = writerV . Step 80

-- Variable size
eBytes :: B.ByteString -> Writer
eBytes = writerV . eBytesS
eLazyBytes :: L.ByteString -> Writer
eLazyBytes = writerV . eLazyBytesS
eShortBytes :: ShortByteString -> Writer
eShortBytes = writerV . eShortBytesS
eNatural :: Natural -> Writer
eNatural = writerV . eNaturalS
eInteger :: Integer -> Writer
eInteger = writerV . eIntegerS
eUTF16 :: Text -> Writer
eUTF16 = writerV . eUTF16S

-- Fixes < 80 size
eChar :: Char -> Writer
eChar = writerF . eCharF
eFloat :: Float -> Writer
eFloat = writerF . eFloatF
eDouble :: Double -> Writer
eDouble = writerF . eDoubleF
eInt64 :: Int64 -> Writer
eInt64 = writerF . eInt64F
eInt32 :: Int32 -> Writer
eInt32 = writerF . eInt32F
eInt16 :: Int16 -> Writer
eInt16 = writerF . eInt16F
eInt8 :: Int8 -> Writer
eInt8 = writerF . eInt8F
eInt :: Int -> Writer
eInt = writerF . eIntF
eWord64 :: Word64 -> Writer
eWord64 = writerF . eWord64F
eWord32 :: Word32 -> Writer
eWord32 = writerF . eWord32F
eWord16 :: Word16 -> Writer
eWord16 = writerF . eWord16F
eWord8 :: Word8 -> Writer
eWord8 = writerF . eWord8F
eWord :: Word -> Writer
eWord = writerF . eWordF
eBits :: NumBits -> Word8 -> Writer
eBits n f = writerF $ eBitsF n f
eFiller :: Writer
eFiller = writerF eFillerF
eBool :: Bool -> Writer
eBool = writerF . eBoolF
eTrue :: Writer
eTrue = writerF eTrueF
eFalse :: Writer
eFalse = writerF eFalseF
