{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |Strict Decoder Primitives
module Data.Flat.Decoder.Prim (
    dBool,
    dWord8,
    dFloat,
    dDouble,
    getChunksInfo,
    dByteString_,
    dLazyByteString_,
    dByteArray_
    ) where

import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Flat.Decoder.Types
import           Data.Flat.Memory
import           Data.FloatCast
import           Data.Word
import           Foreign
import           System.Endian

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = Get $ \endPtr s -> 
  if currPtr s >= endPtr
    then notEnoughSpace endPtr s
    else do
      !w <- peek (currPtr s)
      let !b = 0 /= (w .&. (128 `shiftR` usedBits s))
      -- let b = testBit w (7-usedBits s)
      let !s' = if usedBits s == 7
                  then s { currPtr = currPtr s `plusPtr` 1, usedBits = 0 }
                  else s { usedBits = usedBits s + 1 }
      return $ GetResult s' b

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = Get $ \endPtr s -> do
      ensureBits endPtr s 8
      !w <- if usedBits s == 0
            then peek (currPtr s)
            else do
                   !w1 <- peek (currPtr s)
                   !w2 <- peek (currPtr s `plusPtr` 1)
                   return $ (w1 `unsafeShiftL` usedBits s) .|. (w2 `unsafeShiftR` (8-usedBits s))
      return $ GetResult (s {currPtr=currPtr s `plusPtr` 1}) w

{-# INLINE ensureBits #-}
ensureBits :: Ptr Word8 -> S -> Int -> IO ()
ensureBits endPtr s n = when ((endPtr `minusPtr` currPtr s) * 8 - usedBits s < n) $ notEnoughSpace endPtr s

-- {-# INLINE incBits #-}
-- incBits :: Int -> S -> S
-- incBits 1 s = if usedBits s == 7
--            then s {currPtr=currPtr s `plusPtr` 1,usedBits=0}
--            else s {usedBits=usedBits s+1}

-- incBits 8 s = s {currPtr=currPtr s `plusPtr` 1}

{-# INLINE dFloat #-}
dFloat :: Get Float
dFloat = Get $ \endPtr s -> do
  ensureBits endPtr s 32
  !w <- if usedBits s == 0
        then toBE32 <$> peek (castPtr $ currPtr s)
        else do
           !w1 <- toBE32 <$> peek (castPtr $ currPtr s)
           !(w2::Word8) <- peek (currPtr s `plusPtr` 4)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `unsafeShiftR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 4}) (wordToFloat w)

{-# INLINE dDouble #-}
dDouble :: Get Double
dDouble = Get $ \endPtr s -> do
  ensureBits endPtr s 64
  !w <- if usedBits s == 0
        then toBE64 <$> peek (castPtr $ currPtr s)
        else do
           !w1 <- toBE64 <$> peek (castPtr $ currPtr s)
           !(w2::Word8) <- peek (currPtr s `plusPtr` 8)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `unsafeShiftR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 8}) (wordToDouble w)

dLazyByteString_ :: Get L.ByteString
dLazyByteString_ = L.fromStrict <$> dByteString_

dByteString_ :: Get B.ByteString
dByteString_ = chunksToByteString <$> getChunksInfo

dByteArray_ :: Get (ByteArray,Int)
dByteArray_ = chunksToByteArray <$> getChunksInfo

getChunksInfo :: Get (Ptr Word8, [Int])
getChunksInfo = Get $ \endPtr s -> do

   let getChunks srcPtr l = do
          ensureBits endPtr s 8
          n <- fromIntegral <$> peek srcPtr
          if n==0
            then return (srcPtr `plusPtr` 1,l [])
            else do
              ensureBits endPtr s ((n+1)*8)
              getChunks (srcPtr `plusPtr` (n+1)) (l . (n:))

   when (usedBits s /=0) $ badEncoding endPtr s
   (currPtr',ns) <- getChunks (currPtr s) id
   return $ GetResult (s {currPtr=currPtr'}) (currPtr s `plusPtr` 1,ns)
