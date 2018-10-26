{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables ,NoMonomorphismRestriction #-}
-- |Strict Decoder Primitives
module Data.Flat.Decoder.Prim (
    dBool,
    dWord8,
    dBE8,
    dBE16,
    dBE32,
    dBE64,
    dBEBits8,
    dBEBits16,
    dBEBits32,
    dBEBits64,
    dropBits,
    dFloat,
    dDouble,
    getChunksInfo,
    dByteString_,
    dLazyByteString_,
    dByteArray_,
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

{-# INLINE ensureBits #-}
-- |Ensure that the specified number of bits is available
ensureBits :: Ptr Word8 -> S -> Int -> IO ()
ensureBits endPtr s n = when ((endPtr `minusPtr` currPtr s) * 8 - usedBits s < n) $ notEnoughSpace endPtr s

{-# INLINE dropBits #-}
-- |Drop the specified number of bits
dropBits :: Int -> Get ()
dropBits n
  | n > 0 = Get $ \endPtr s -> do
      ensureBits endPtr s n
      return $ GetResult (dropBits_ s n) ()
  | n == 0 = return ()
  | otherwise = error $ unwords ["dropBits",show n]

dropBits_ :: S -> Int -> S
dropBits_ s n = let (bytes,bits) = (n+usedBits s) `divMod` 8
                in S {currPtr=currPtr s `plusPtr` bytes,usedBits=bits}

{-# INLINE dBool #-}
-- |Decode a boolean
dBool :: Get Bool
dBool = Get $ \endPtr s ->
  if currPtr s >= endPtr
    then notEnoughSpace endPtr s
    else do
      !w <- peek (currPtr s)
      let !b = 0 /= (w .&. (128 `shR` usedBits s))
      -- let b = testBit w (7-usedBits s)
      let !s' = if usedBits s == 7
                  then s { currPtr = currPtr s `plusPtr` 1, usedBits = 0 }
                  else s { usedBits = usedBits s + 1 }
      return $ GetResult s' b

{-# INLINE dBEBits8  #-}
-- |Return the n most significant bits (up to maximum of 8)
--
-- The bits are returned right shifted:
--
-- unflatWith (dBEBits8 3) [128+64+32+1::Word8] == Right 7
dBEBits8 :: Int -> Get Word8
dBEBits8 n = Get $ \endPtr s -> do
      ensureBits endPtr s n
      take8 s n

{-# INLINE dBEBits16  #-}
-- |Return the n most significant bits (up to maximum of 16)
-- The bits are returned right shifted.
dBEBits16 :: Int -> Get Word16
dBEBits16 n = Get $ \endPtr s -> do
      ensureBits endPtr s n
      takeN n s

{-# INLINE dBEBits32  #-}
-- |Return the n most significant bits (up to maximum of 8)
-- The bits are returned right shifted.
dBEBits32 :: Int -> Get Word32
dBEBits32 n = Get $ \endPtr s -> do
      ensureBits endPtr s n
      takeN n s

{-# INLINE dBEBits64  #-}
-- |Return the n most significant bits (up to maximum of 8)
-- The bits are returned right shifted.
dBEBits64 :: Int -> Get Word64
dBEBits64 n = Get $ \endPtr s -> do
      ensureBits endPtr s n
      takeN n s

-- {-# INLINE take8 #-}
-- take8 :: Int -> S -> IO (GetResult Word8)
-- take8 n s
--   | n == 0 = return $ GetResult s 0

--   -- all bits in the same byte
--   | n <= 8 - usedBits s = do
--       w <- peek (currPtr s)
--       let (bytes,bits) = (n+usedBits s) `divMod` 8
--       return $ GetResult (S {currPtr=currPtr s `plusPtr` bytes,usedBits=bits}) ((w `unsafeShiftL` usedBits s) `shR` (8 - n))

--   -- two different bytes
--   | n <= 8 = do
--       w::Word16 <- toBE16 <$> peek (castPtr $ currPtr s)
--       return $ GetResult (S {currPtr=currPtr s `plusPtr` 1,usedBits=(usedBits s + n) `mod` 8}) (fromIntegral $ (w `unsafeShiftL` usedBits s) `shR` (16 - n))

--   | otherwise = error $ unwords ["take8: cannot take",show n,"bits"]

{-# INLINE take8 #-}
take8 :: S -> Int -> IO (GetResult Word8)
take8 s n = GetResult (dropBits_ s n) <$> read8 s n

{-# INLINE read8 #-}
read8 :: S -> Int -> IO Word8
read8 s n | n >=0 && n <=8 =
            if n <= 8 - usedBits s
            then do  -- all bits in the same byte
              w <- peek (currPtr s)
              return $ (w `unsafeShiftL` usedBits s) `shR` (8 - n)
            else do -- two different bytes
              w::Word16 <- toBE16 <$> peek (castPtr $ currPtr s)
              return $ fromIntegral $ (w `unsafeShiftL` usedBits s) `shR` (16 - n)
          | otherwise = error $ unwords ["read8: cannot read",show n,"bits"]

{-# INLINE takeN #-}
takeN :: (Num a, Bits a) => Int -> S -> IO (GetResult a)
takeN n s = read s 0 (n - (n `min` 8)) n
   where
     read s r sh n | n <=0 = return $ GetResult s r
                   | otherwise = do
                     let m = n `min` 8
                     GetResult s' b <- take8 s m
                     read s' (r .|. (fromIntegral b `unsafeShiftL` sh)) ((sh-8) `max` 0) (n-8)

-- takeN n = Get $ \endPtr s -> do
--   ensureBits endPtr s n
--   let (bytes,bits) = (n+usedBits s) `divMod` 8
--   r <- case bytes of
--     0 -> do
--       w <- peek (currPtr s)
--       return . fromIntegral $ ((w `unsafeShiftL` usedBits s) `shR` (8 - n))
--     1 -> do
--       w::Word16 <- toBE16 <$> peek (castPtr $ currPtr s)
--       return $ fromIntegral $ (w `unsafeShiftL` usedBits s) `shR` (16 - n)
--     2 -> do
--       let r = 0
--       w1 <- fromIntegral <$> r8 s
--       w2 <- fromIntegral <$> r16 s
--       w1 
--   return $ GetResult (S {currPtr=currPtr s `plusPtr` bytes,usedBits=bits}) r

-- r8 s = peek (currPtr s)
-- r16 s = toBE16 <$> peek (castPtr $ currPtr s)

-- |Return the 8 most significant bits (same as dBE8)
dWord8 :: Get Word8
dWord8 = dBE8

{-# INLINE dBE8  #-}
-- |Return the 8 most significant bits
dBE8 :: Get Word8
dBE8 = Get $ \endPtr s -> do
      ensureBits endPtr s 8
      !w1 <- peek (currPtr s)
      !w <- if usedBits s == 0
            then return w1
            else do
                   !w2 <- peek (currPtr s `plusPtr` 1)
                   return $ (w1 `unsafeShiftL` usedBits s) .|. (w2 `shR` (8-usedBits s))
      return $ GetResult (s {currPtr=currPtr s `plusPtr` 1}) w

{-# INLINE dBE16 #-}
-- |Return the 16 most significant bits
dBE16 :: Get Word16
dBE16 = Get $ \endPtr s -> do
  ensureBits endPtr s 16
  !w1 <- toBE16 <$> peek (castPtr $ currPtr s)
  !w <- if usedBits s == 0
        then return w1
        else do
           !(w2::Word8) <- peek (currPtr s `plusPtr` 2)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `shR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 2}) w

{-# INLINE dBE32 #-}
-- |Return the 32 most significant bits
dBE32 :: Get Word32
dBE32 = Get $ \endPtr s -> do
  ensureBits endPtr s 32
  !w1 <- toBE32 <$> peek (castPtr $ currPtr s)
  !w <- if usedBits s == 0
        then return w1
        else do
           !(w2::Word8) <- peek (currPtr s `plusPtr` 4)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `shR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 4}) w

{-# INLINE dBE64 #-}
-- |Return the 64 most significant bits
dBE64 :: Get Word64
dBE64 = Get $ \endPtr s -> do
  ensureBits endPtr s 64
  -- !w1 <- toBE64 <$> peek (castPtr $ currPtr s)
  !w1 <- toBE64 <$> peek64 (castPtr $ currPtr s)
  !w <- if usedBits s == 0
        then return w1
        else do
           !(w2::Word8) <- peek (currPtr s `plusPtr` 8)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `shR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 8}) w

{-# INLINE peek64 #-}
peek64 :: Ptr Word64 -> IO Word64
peek64 ptr = fix64 <$> peek ptr 

-- #ifdef ghcjs_HOST_OS
-- peek64 ptr = (`rotateR` 32) <$> peek ptr 
-- #else
-- peek64 = peek
-- #endif

{-# INLINE dFloat #-}
-- |Decode a Float
dFloat :: Get Float
dFloat = wordToFloat <$> dBE32

{-# INLINE dDouble #-}
-- |Decode a Double
dDouble :: Get Double
dDouble = wordToDouble <$> dBE64

-- |Decode a Lazy ByteString
dLazyByteString_ :: Get L.ByteString
dLazyByteString_ = L.fromStrict <$> dByteString_

-- |Decode a ByteString
dByteString_ :: Get B.ByteString
dByteString_ = chunksToByteString <$> getChunksInfo

-- |Decode a ByteArray and its length
dByteArray_ :: Get (ByteArray,Int)
dByteArray_ = chunksToByteArray <$> getChunksInfo

-- |Decode an Array (a list of chunks up to 255 bytes long)
-- returning the pointer to the first data byte and a list of chunk sizes
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

   when (usedBits s /=0) $ badEncoding endPtr s "usedBits /= 0"
   (currPtr',ns) <- getChunks (currPtr s) id
   return $ GetResult (s {currPtr=currPtr'}) (currPtr s `plusPtr` 1,ns)

{-# INLINE shR #-}
shR :: Bits a => a -> Int -> a
#ifdef ghcjs_HOST_OS
shR val 0 = val
shR val n = shift val (-n)

-- shR = unsafeShiftR
#else
shR = unsafeShiftR  
#endif      
