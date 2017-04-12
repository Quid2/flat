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
import Data.Flat.Pokes hiding (eBitsS,E)
import qualified Data.Flat.Pokes as P
import Debug.Trace

-- Lazy encoder
type Encoding = Writer

type Contexts = [Writer]

-- newtype vs data, way faster for nested structures, slower for some fixed size ones
newtype Writer = Writer {runWriter :: Contexts -> E -> IO E}
instance Show Writer where show (Writer _) = "Writer"

{-
At the beginning of every constructor, perform a minimum check assuming that are the costructors parameters are all fixed size primitives with max size = 80, so check for 80*numPars bits.

Fixed size primitives do not need to perform any check.

Variable size primitives, are passed.
-}

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderLazy 64 encoder

encoder :: E -> Encoding -> IO (Signal Encoding)
encoder e@(E p s) w = catch (runWriter w [] e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (encodersS (traceShow (unwords ["encoder",show ws]) ws)))

data NotEnoughSpaceException = NotEnoughSpaceException S Int Contexts deriving Show
instance Exception NotEnoughSpaceException

{-
  Cons !            a
       (Cons !   b)
              Nil c

Nil -> [[c],[b],[a]] -> [[Nil,c],[b],[a]]
-}

{-# INLINE encodersS #-}
encodersS :: [Writer] -> Writer
-- without the explicit parameter the rules won't fire
-- encodersS ws = checkedWriter $ encoders 0 ws
encodersS ws = Writer $ \ks -> runWriter (writerN (length ws * 80)) (ws++ks) >=> encoders ks ws

-- encoders k [] = return
-- encoders k (h:t) = runWriter h k >=> encoders (k+1) t

--encoders ks [] = return
encoders ks [w] = runWriter w ks
encoders ks (h:t) = runWriter h (t++ks) >=> encoders ks t

{-# INLINE writerF #-}
writerF f = Writer $ \_ (E p s) -> f s >>= return . E p

{-# INLINE writerV #-}
writerV :: Step -> Writer
writerV (Step n f) = me
  where
    me = Writer prim
    prim ks e@(E p s) | n <= availBits e = f s >>= return . E p
                      | otherwise = throw (NotEnoughSpaceException s n (me:ks))

{-# INLINE writerN #-}
writerN n = writerV (Step n return)

-- writerN n = me
--   where
--     me = Writer prim
--     prim k e@(E p s) | n <= availBits e = return e
--                      | otherwise = throw (NotEnoughSpaceException s n (me:k))

--checkSize e@(E p s) | n <= availBits e

{- # RULES
-- "encodersSN" forall h t. encodersS (h:t) = Writer $ \k-> runWriter h (t++k) >=> encodersS t
"encodersS3" forall a1 a2 a3. encodersS [a1,a2,a3] = Writer $ \k -> runWriter a1 (a2:a3:k) >=> runWriter a2 (a3:k) >=> runWriter a3 k
-- "encodersS2" forall a1 a2. encodersS [a1,a2] = Writer $ \k -> runWriter a1 (a2:k) >=> runWriter a2 k
-- "encodersS2" forall a1 a2. encodersS [a1,a2] = Writer $ \k -> runWriter a1 (a2:k) >=> runWriter a2 k
-- "encodersS2" forall a1 a2. encodersS l@[a1,a2] = writerN (length l * 80) [a1,a2] >=> runWriter a1 [a2] >=> runWriter a2 []
-- "encodersS1" forall a. encodersS [a] = a
-- "encodersS0" encodersS [] = mempty
 # -}

-- {-# INLINE checkedWriter #-}
-- checkedWriter ws w = Writer $ \(E p s) ->
--   let neededBits = length ws * 80
--   in if availBits e < neededBits
--      then notEnoughSpace s neededBits (encoders (traceShow (unwords ["encoder",show ws]) ws)))
--      else w e -- catch (w [] e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (encoders (traceShow (unwords ["encoder",show ws]) ws)))

--data Context = Context []


-- encodersS ws = error $ unwords ["encodersS CALLED",show ws]

-- class Checked c where checked :: c -> Writer

eChar :: Char -> Writer
eChar = writerF . eCharF
eUTF16 :: Text -> Writer
eUTF16 = writerF . eUTF16F
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
