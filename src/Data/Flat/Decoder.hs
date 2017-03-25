{-# LANGUAGE CPP ,BangPatterns #-}
module Data.Flat.Decoder (
    Get,
    dByteString,
    dLazyByteString,
    dShortByteString,
    dShortByteString_,
    dUTF16,
    dUTF8,
    dArray,
    dFloat,
    dDouble,
    dInteger,
    dNatural,
    dChar,
    dBool,
    dWord8,
    dWord16,
    dWord32,
    dWord64,
    dWord,
    dInt8,
    dInt16,
    dInt32,
    dInt64,
    dInt,
    runGetLazy,
    runGetRawLazy,
    ) where

import           Data.Flat.Peeks
import           Data.Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.DList            as DL
import           Data.Int
import qualified Data.Text    as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as TA
import           Data.Word
import           Data.ZigZag
import           Numeric.Natural
import           Data.Primitive.ByteArray
import GHC.Base(unsafeChr)
import GHC.Magic(oneShot)
-- import Data.Char(chr)

#include "MachDeps.h"

{-# INLINE dNatural #-}
dNatural :: Get Natural
dNatural = fromInteger <$> dUnsigned

{-# INLINE dInteger #-}
dInteger :: Get Integer
dInteger = zzDecodeInteger <$> (dUnsigned::Get Integer)

{-# INLINE dWord  #-}
{-# INLINE dInt  #-}
dWord :: Get Word
dInt :: Get Int

#if WORD_SIZE_IN_BITS == 64
dWord = (fromIntegral :: Word64 -> Word) <$> dWord64
dInt = (fromIntegral :: Int64 -> Int) <$> dInt64

#elif WORD_SIZE_IN_BITS == 32
dWord = (fromIntegral :: Word32 -> Word) <$> dWord32
dInt = (fromIntegral :: Int32 -> Int) <$> dInt32

#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

{-# INLINE dInt8  #-}
dInt8 :: Get Int8
dInt8 = zzDecode8 <$> dWord8

{-# INLINE dInt16  #-}
dInt16 :: Get Int16
dInt16 = zzDecode16 <$> dWord16

{-# INLINE dInt32  #-}
dInt32 :: Get Int32
dInt32 = zzDecode32 <$> dWord32

{-# INLINE dInt64  #-}
dInt64 :: Get Int64
dInt64 = zzDecode64 <$> dWord64

-- {-# INLINE dWord16  #-}
dWord16 :: Get Word16
dWord16 = wordStep 0 (wordStep 7 (lastStep 14)) 0

-- {-# INLINE dWord32  #-}
dWord32 :: Get Word32
dWord32 = wordStep 0 (wordStep 7 (wordStep 14 (wordStep 21 (lastStep 28)))) 0

-- {-# INLINE dWord64  #-}
dWord64 :: Get Word64
dWord64 = wordStep 0 (wordStep 7 (wordStep 14 (wordStep 21 (wordStep 28 (wordStep 35 (wordStep 42 (wordStep 49 (wordStep 56 (wordStep 63 (wordStep 70 (lastStep 77))))))))))) 0


{-
-- data Unicode = Unicode Word32
instance Flat Char where
  encode c = encode (fromIntegral . ord $ c :: Word32)

  decode = do
    w :: Word32 <- decode
    if w > 0x10FFFF
      then error $ "Not a valid Unicode code point: " ++ show w
      else return . chr .fromIntegral $ w
-}


{-# INLINE dChar #-}
dChar :: Get Char
-- dChar = chr . fromIntegral <$> dWord32

-- Not really faster than the simple version above
dChar = charStep 0 (charStep 7 (lastCharStep 14)) 0

{-# INLINE charStep #-}
charStep :: Int -> (Int -> Get Char) -> Int -> Get Char
charStep !shl !cont !n = do
  !tw <- fromIntegral <$> dWord8
  let !w = tw .&. 127
  let !v = n .|. (w `shift` shl)
  if tw == w
    then return $ unsafeChr v
    else cont v

{-# INLINE lastCharStep #-}
lastCharStep :: Int -> Int -> Get Char
lastCharStep !shl !n = do
  !tw <- fromIntegral <$> dWord8
  let !w = tw .&. 127
  let !v = n .|. (w `shift` shl)
  if tw == w
    then if v > 0x10FFFF
         then charErr v
         else return $ unsafeChr v
    else charErr v

charErr :: (Show a1, Monad m) => a1 -> m a
charErr v = fail $ concat ["Unexpected extra byte or non unicode char",show v]

{-# INLINE wordStep #-}
wordStep
  :: (Bits a, Num a) => Int -> (a -> Get a) -> a -> Get a
wordStep shl k n = do
  tw <- fromIntegral <$> dWord8
  let w = tw .&. 127
  let v = n .|. (w `shift` shl)
  if tw == w
    then return v
    --else oneShot k v
    else k v

{-# INLINE lastStep #-}
lastStep :: (FiniteBits b, Show b, Num b) => Int -> b -> Get b
lastStep shl n = do
  tw <- fromIntegral <$> dWord8
  let w = tw .&. 127
  let v = n .|. (w `shift` shl)
  if tw == w
    then if countLeadingZeros w < shl
         then wordErr v
         else return v
    else wordErr v

wordErr :: (Show a1, Monad m) => a1 -> m a
wordErr v = fail $ concat ["Unexpected extra byte in unsigned integer",show v]

-- {-# INLINE dUnsigned #-}
dUnsigned :: (Num b, Bits b) => Get b
dUnsigned = do
  (v,shl) <- dUnsigned_ 0 0
  -- return v
  maybe (return v) (\s -> if shl>= s then fail "Unexpected extra data in unsigned integer" else return v) $ bitSizeMaybe v

-- {-# INLINE dUnsigned_ #-}
dUnsigned_ :: (Bits t, Num t) => Int -> t -> Get (t, Int)
dUnsigned_ shl n = do
  tw <- dWord8
  let w = tw .&. 127
  let v = n .|. (fromIntegral w `shift` shl)
  if tw == w
    then return (v,shl)
    else dUnsigned_ (shl+7) v

dArray :: Get a -> Get [a]
dArray dec = DL.toList <$> getAsL_ dec

-- TODO: test if it would it be faster with DList.unfoldr :: (b -> Maybe (a, b)) -> b -> Data.DList.DList a
--  getAsL_ :: Flat a => Get (DL.DList a)
getAsL_ :: Get a -> Get (DL.DList a)
getAsL_ dec = do
    tag <- dWord8
    case tag of
         0 -> return DL.empty
         _ -> do
           h <- gets tag
           t <- getAsL_ dec
           return (DL.append h t)

  where
    gets 0 = return DL.empty
    gets n = DL.cons <$> dec <*> gets (n-1)

--encode = encode . blob UTF8Encoding . L.fromStrict . T.encodeUtf8
--decode = T.decodeUtf8 . L.toStrict . (unblob :: BLOB UTF8Encoding -> L.ByteString) <$> decode

-- BLOB UTF16Encoding
dUTF16 :: Get T.Text
dUTF16 = do
  _ <- dFiller
  -- Checked decoding
  -- T.decodeUtf16LE <$> dByteString_
  -- Unchecked decoding
  (ByteArray array,lengthInBytes) <- dByteArray_
  return (T.Text (TA.Array array) 0 (lengthInBytes `div` 2))

dUTF8 :: Get T.Text
dUTF8 = do
  _ <- dFiller
  T.decodeUtf8 <$> dByteString_

dFiller :: Get ()
dFiller = do
  tag <- dBool
  case tag of
    False -> dFiller
    True  -> return ()

dLazyByteString :: Get L.ByteString
dLazyByteString = dFiller >> dLazyByteString_

dShortByteString :: Get SBS.ShortByteString
dShortByteString = dFiller >> dShortByteString_

dShortByteString_ :: Get SBS.ShortByteString
dShortByteString_ = do
  (ByteArray array,_) <- dByteArray_
  return $ SBS.SBS array

dByteString :: Get B.ByteString
dByteString = dFiller >> dByteString_
