{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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

    ConsState(..),consOpen,consClose,consBool,consBits
    ) where

import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Flat.Decoder.Types
import           Data.Flat.Endian
import           Data.Flat.Memory
import           Data.FloatCast
import           Data.Word
import           Foreign

-- $setup
-- >>> :set -XBinaryLiterals
-- >>> import Data.Flat.Run

-- |A special state, optimised for constructor decoding, consists of:
-- The bits to parse, top bit being the first to parse (could use a Word16 instead, no difference in performance)
-- The number of decoded bits
-- Supports up to 512 constructors (9 bits)
data ConsState =
  ConsState {-# UNPACK #-} !Word !Int

-- |Switch to constructor decoding
-- {-# INLINE consOpen  #-}
consOpen :: Get ConsState
consOpen = Get $ \endPtr s -> do
  let u = usedBits s
  w <- case compare (currPtr s) endPtr of
    LT -> do -- two different bytes
      w16::Word16 <- toBE16 <$> peek (castPtr $ currPtr s)
      return $ fromIntegral w16 `unsafeShiftL` (u+(wordSize-16))
    EQ -> do
        w8 :: Word8 <- peek (currPtr s)
        return $ fromIntegral w8 `unsafeShiftL` (u+(wordSize-8))
    GT -> notEnoughSpace endPtr s
  return $ GetResult s (ConsState w 0)

-- |Switch back to normal decoding
-- {-# NOINLINE consClose  #-}
consClose :: Int -> Get ()
consClose n =  Get $ \endPtr s -> do
  let u' = n+usedBits s
  if u' < 8
     then return $ GetResult (s {usedBits=u'}) ()
     else if currPtr s >= endPtr
            then notEnoughSpace endPtr s
          else return $ GetResult (s {currPtr=currPtr s `plusPtr` 1,usedBits=u'-8}) ()

  {- ensureBits endPtr s n = when ((endPtr `minusPtr` currPtr s) * 8 - usedBits s < n) $ notEnoughSpace endPtr s
  dropBits8 s n =
    let u' = n+usedBits s
    in if u' < 8
        then s {usedBits=u'}
        else s {currPtr=currPtr s `plusPtr` 1,usedBits=u'-8}
  -}

  --ensureBits endPtr s n
  --return $ GetResult (dropBits8 s n) ()

-- |Decode a single bit
consBool :: ConsState -> (ConsState,Bool)
consBool cs =  (0/=) <$> consBits cs 1

-- consBool (ConsState w usedBits) = (ConsState (w `unsafeShiftL` 1) (1+usedBits),0 /= 32768 .&. w)

-- |Decode from 1 to 3 bits
-- This could read more bits that are available, but it doesn't matter, errors will be checked in consClose
consBits :: ConsState -> Int -> (ConsState, Word)
consBits cs 3 = consBits_ cs 3 7
consBits cs 2 = consBits_ cs 2 3
consBits cs 1 = consBits_ cs 1 1
consBits _  _ = error "unsupported"

consBits_ :: ConsState -> Int -> Word -> (ConsState, Word)

-- Different decoding primitives
-- All with equivalent performance
-- #define CONS_ROT
-- #define CONS_SHL
#define CONS_STA

#ifdef CONS_ROT
consBits_ (ConsState w usedBits) numBits mask =
  let usedBits' = numBits+usedBits
      w' = w `rotateL` numBits -- compiles to an or+shiftl+shiftr
  in (ConsState w' usedBits',w' .&. mask)
#endif

#ifdef CONS_SHL
consBits_ (ConsState w usedBits) numBits mask =
  let usedBits' = numBits+usedBits
      w' = w `unsafeShiftL` numBits
  in (ConsState w' usedBits', (w `shR` (wordSize - numBits)) .&. mask)
#endif

#ifdef CONS_STA
consBits_ (ConsState w usedBits) numBits mask =
  let usedBits' = numBits+usedBits
  in (ConsState w usedBits', (w `shR` (wordSize - usedBits')) .&. mask)
#endif

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

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

{-# INLINE dropBits_ #-}
dropBits_ :: S -> Int -> S
dropBits_ s n =
  let (bytes,bits) = (n+usedBits s) `divMod` 8
  -- let
  --   n' = n+usedBits s
  --   bytes = n' `shR` 3
  --   bits = n' .|. 7
  in S {currPtr=currPtr s `plusPtr` bytes,usedBits=bits}

{-# INLINE dBool #-}
-- {-# INLINE dBool #-} -- INLINE Massively increases compilation time and decreases run time by a third
-- |Decode a boolean
dBool :: Get Bool
dBool = Get $ \endPtr s ->
  if currPtr s >= endPtr
    then notEnoughSpace endPtr s
    else do
      !w <- peek (currPtr s)
      let !b = 0 /= (w .&. (128 `shR` usedBits s))
      let !s' = if usedBits s == 7
                  then s { currPtr = currPtr s `plusPtr` 1, usedBits = 0 }
                  else s { usedBits = usedBits s + 1 }
      return $ GetResult s' b


{-# INLINE dBEBits8  #-}
-- |Return the n most significant bits (up to maximum of 8)
--
-- The bits are returned right shifted:
-- >>> unflatWith (dBEBits8 3) [0b11100001::Word8] == Right 0b00000111
-- True
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
-- take8 s n = GetResult (dropBits_ s n) <$> read8 s n
take8 s n = GetResult (dropBits8 s n) <$> read8 s n
  where
    --{-# INLINE read8 #-}
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
    -- {-# INLINE dropBits8 #-}
    -- -- Assume n <= 8
    dropBits8 :: S -> Int -> S
    dropBits8 s n =
      let u' = n+usedBits s
      in if u' < 8
          then s {usedBits=u'}
          else s {currPtr=currPtr s `plusPtr` 1,usedBits=u'-8}


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
    where
      -- {-# INLINE peek64 #-}
      peek64 :: Ptr Word64 -> IO Word64
      peek64 ptr = fix64 <$> peek ptr

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
          !n <- fromIntegral <$> peek srcPtr
          if n==0
            then return (srcPtr `plusPtr` 1,l [])
            else do
              ensureBits endPtr s ((n+1)*8)
              getChunks (srcPtr `plusPtr` (n+1)) (l . (n:)) -- eta: stck overflow (missing tail call optimisation)

   when (usedBits s /=0) $ badEncoding endPtr s "usedBits /= 0"
   (currPtr',ns) <- getChunks (currPtr s) id
   return $ GetResult (s {currPtr=currPtr'}) (currPtr s `plusPtr` 1,ns)

-- Fix for ghcjs bug:  https://github.com/ghcjs/ghcjs/issues/706
-- TODO: verify if actually needed here and if also needed in encoder
{-# INLINE shR #-}
shR :: Bits a => a -> Int -> a
#ifdef ghcjs_HOST_OS
shR val 0 = val
shR val n = shift val (-n)
#else
shR = unsafeShiftR
#endif
