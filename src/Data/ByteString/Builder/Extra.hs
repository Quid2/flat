{-# LANGUAGE BangPatterns #-}
{- Extension of Data.ByteString.Builder and related stuff -}
-- TODO: Check latest Data.ByteString.Builder.Extra from bytestring
module Data.ByteString.Builder.Extra(Builder,toLazyByteString,L.ByteString,BufferRange(..),PI.runB,builder,bword8,bword64,bwordUpTo64,bufferFull
                   ,tag8,tag,w1,w2,w3,w4,w5,w6,w7,w8,numBytes
                   ,byteString,lazyByteString
                   ,PI.BoundedPrim,stringB,boolP
                   ,putN,putNB
                   ,fixed1,bufM) where

import qualified Data.ByteString.Lazy                           as L

-- import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder
import Data.ByteString.Builder.Internal
import qualified Data.ByteString.Builder.Prim                  as P
import           Data.ByteString.Builder.Prim  ( (>$<), (>*<), condB )
import qualified Data.ByteString.Builder.Prim.Internal         as PI
-- import           Foreign.Ptr
import Data.Word
import Data.Shift
import Control.Monad(when)
import           GHC.Ptr(Ptr,plusPtr)
import           Foreign.Storable(poke)
import Data.Char

stringB :: [Char] -> Builder
stringB = putN charP

putN :: PI.BoundedPrim a -> [a] -> Builder
putN = putN_ 0

putN_ :: Word8 -> PI.BoundedPrim a -> [a] -> Builder
putN_ n w l = builder $ step n l
  where
    bound = 2 + 255 * PI.sizeBound  w
    step n l k (BufferRange op0 ope0) = go n l op0 op0
      where
        go n l op0 !op
          | op `plusPtr` bound <= ope0  = zeroW op >>= next n l op0
          | otherwise                   = return $ bufferFull bound op (step n l k)

        next 0   []    op0 !op = k (BufferRange op ope0)
        next n   []    op0 !op = poke op0 n >> zeroW op >>= \op -> k (BufferRange op ope0)
        next 255 l     op0 !op = poke op0 255 >> go 0 l  op  op
        next n   (h:t) op0 !op = PI.runB w h op >>= next (n+1) t op0

{-
putN_ n w xs0 = builder $ step n xs0
  where
    bound = max 1 $ PI.sizeBound  w
    w0 = PI.runB (fixed1 P.word8) 0
    step n xs1 k (BufferRange op0 ope0) = go n xs1 op0 op0
      where
        go 255 l op0  !op  = poke op0 255 >> go 0 l op op

        go n xs op0 !op
          | op `plusPtr` bound <= ope0 =
            do
              op' <- if n==0
                     then w0 op
                     else return op
              case xs of
                (x':xs') -> PI.runB w x' op' >>= go (n+1) xs' op0
                [] -> if n==0
                      then k (BufferRange op' ope0)
                      else poke op0 n >> go 0 [] op' op'
          | otherwise                  =
             return $ bufferFull bound op (step n xs k)
-}

{-# INLINE zeroW #-}
zeroW :: Ptr Word8 -> IO (Ptr Word8)
-- zeroW = PI.runB (fixed1 P.word8) 0
zeroW p = poke p (0::Word8) >> return (p `plusPtr` 1)

putNB :: (a -> Builder) -> [a] -> Builder
putNB = putNB_ 0

putNB_ n w xs0 = builder $ step n xs0
  where
    w0 = PI.runB (PI.toB P.word8) 0
    bound = 1
    step n xs1 k (BufferRange op0 ope0) = go n xs1 op0 op0
      where
        go 255 l op0  !op  = poke op0 255 >> go 0 l op op

        go n xs op0 !op
          | op `plusPtr` bound <= ope0 =
            do
              op' <- if n==0
                     then w0 op
                     else return op
              case xs of
                (x':xs') -> runBuilderWith (w x') (step (n+1) xs' k) (BufferRange op' ope0)
                [] -> if n==0
                      then k (BufferRange op' ope0)
                      else poke op0 n >> go 0 [] op' op'
          | otherwise                  =
             return $ bufferFull bound op (step n xs k)


{-# INLINE w1 #-}
w1 = tag 8

{-# INLINE w2 #-}
w2 = tag 16

{-# INLINE w3 #-}
w3 = tag 24

{-# INLINE w4 #-}
w4 = tag 32

{-# INLINE w5 #-}
w5 = tag 40

{-# INLINE w6 #-}
w6 = tag 48

{-# INLINE w7 #-}
w7 = tag 56

{-# INLINE w8 #-}
w8 = tag 64

{-# INLINE tag #-}
tag :: Int -> Word64 -> Builder
tag numBits = {-# SCC tag #-} P.primBounded (tagM numBits)

{-# INLINE tag8 #-}
tag8 :: Int -> Word8 -> Builder
tag8 numBits = {-# SCC tag8 #-} P.primBounded bword8

{-# INLINE bufM #-}
-- 64 Buffer for bit coder, filled starting with MSB.
-- Prob: has to make all checks every time
bufM :: Int -> P.BoundedPrim Word64
bufM o  = fixed1 $ PI.fixedPrim n $ \w p -> do
  when (n>=1) $ poke p (fromIntegral (shiftr_w64 w 56) :: Word8)
  when (n>=2) $ poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
  when (n>=3) $ poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
  when (n>=4) $ poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
  when (n>=5) $ poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
  when (n>=6) $ poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
  when (n>=7) $ poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 8) :: Word8)
  when (n==8) $ poke (p `plusPtr` 7) (fromIntegral w :: Word8)
    where n = numBytes o

numBytes n = let (d,m) = n `divMod` 8 in d + if m>0 then 1 else 0

{-# INLINE bword8 #-}
bword8 :: P.BoundedPrim Word8
bword8 = fixed1 P.word8

{-# INLINE bword64 #-}
bword64 :: P.BoundedPrim Word64
bword64 = fixed1 P.word64BE

{-# INLINE bwordUpTo64 #-}
-- BUG: we assume n in 1..64
bwordUpTo64 :: Int -> P.BoundedPrim Word64
bwordUpTo64 n = let b = (n-1) `div` 8 + 1
                in fixed1 $ PI.fixedPrim b $ \w p -> do
                  poke p (fromIntegral (shiftr_w64 w 56) :: Word8)
                  when (b>1) $ poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
                  when (b>2) $ poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
                  when (b>3) $ poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
                  when (b>4) $ poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
                  when (b>5) $ poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
                  when (b>6) $ poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 8) :: Word8)
                  when (b>7) $ poke (p `plusPtr` 7) (fromIntegral w :: Word8)

{-# INLINE fixed1 #-}
fixed1 :: P.FixedPrim a -> P.BoundedPrim a
fixed1 = PI.toB -- liftFixedToBounded

{-# INLINE preFixed1 #-}
preFixed1 :: Word8 -> P.FixedPrim a -> P.BoundedPrim a
preFixed1 header p = (\x -> (header, x)) >$< fixed1 (P.word8 >*< p)

-- TODO: use Cond ..?
{-# INLINE tagM #-}
tagM :: Int -> P.BoundedPrim Word64
tagM n        | n <= 8  = fixed1 (fromIntegral >$< P.word8)
              | n <= 16 = fixed1 (fromIntegral >$< P.word16BE)
              | n <= 24 = fixed1 (fromIntegral >$< word24BE)
              | n <= 32 = fixed1 (fromIntegral >$< P.word32BE)
              | n <= 40 = fixed1 (fromIntegral >$< word40BE)
              | n <= 48 = fixed1 (fromIntegral >$< word48BE)
              | n <= 56 = fixed1 (fromIntegral >$< word56BE)
              | otherwise = fixed1 (fromIntegral >$< P.word64BE)

{-
{-# INLINE tagMP #-}
tagMP :: Word64 -> P.BoundedPrim Word64
tagMP totTags | totTags <= 0xFF   = fixed1 (fromIntegral >$< P.word8)
              | totTags <= 0xFFFF = fixed1 (fromIntegral >$< P.word16BE)
              | totTags <= 0xFFFFFF = fixed1 (fromIntegral >$< word24BE)
              | totTags <= 0xFFFFFFFF = fixed1 (fromIntegral >$< P.word32BE)
              | totTags <= 0xFFFFFFFFFF = fixed1 (fromIntegral >$< word40BE)
              | totTags <= 0xFFFFFFFFFFFF = fixed1 (fromIntegral >$< word48BE)
              | totTags <= 0xFFFFFFFFFFFFFF = fixed1 (fromIntegral >$< word56BE)
              | otherwise = fixed1 (fromIntegral >$< P.word64BE)
              -- | otherwise = error "Supports only up to 2**16 constructors"
-}

charP :: P.BoundedPrim Char
charP = (fromIntegral . ord) >$< word32P

-- Bool Enum
boolP :: P.BoundedPrim Bool
-- boolP =  fixed1 ((fromIntegral . fromEnum) >$< P.word8)

boolP = (fromIntegral . fromEnum) >$< (tagM 2)

word32P :: P.BoundedPrim Word32
word32P =
    condB (<= 0xFC)       (   fixed1      (fromIntegral >$< P.word8   )) $
    condB (<= 0xFFFF)     (preFixed1 0xFD (fromIntegral >$< P.word16BE)) $
    condB (<= 0xFFFFFF)   (preFixed1 0xFE (fromIntegral >$<  word24BE)) $
                          (preFixed1 0xFF                   P.word32BE )

{-# INLINE word24BE #-}
word24BE :: P.FixedPrim Word32
word24BE = PI.fixedPrim 3 $ \w p -> do
    poke p               (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 8)  :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (w)               :: Word8)

{-# INLINE word40BE #-}
word40BE :: P.FixedPrim Word64
word40BE = PI.fixedPrim 5 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (w)               :: Word8)

{-# INLINE word48BE #-}
word48BE :: P.FixedPrim Word64
word48BE = PI.fixedPrim 6 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (w)               :: Word8)

{-# INLINE word56BE #-}
word56BE :: P.FixedPrim Word64
word56BE = PI.fixedPrim 7 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (w)               :: Word8)
{-
{-# INLINE word24LE #-}
word24LE = fixedPrim 3 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)

{-# INLINE word40LE #-}
word40LE = fixedPrim 5 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)

{-# INLINE word24LE #-}
word64LE = fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
{-# INLINE word24LE #-}
word64LE = fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
{-# INLINE word24LE #-}
word64LE = fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
-}

-- TODO: See ~/cache/haskell/network/serialize/bytes-0.13.0.1/src/Data/Bytes/VarInt.hs
{-
-- Copied from protocol-buffers-2.0.14 Text.ProtocolBuffers
getVarWord :: (Integral a, Bits a) => Get k a
{-# INLINE getVarWord #-}
getVarWord = do -- optimize first read instead of calling (go 0 0)
  b <- getWord8
  if testBit b 7 then go 7 (fromIntegral (b .&. 0x7F))
    else return (fromIntegral b)
 where
  go n val = do
    b <- getWord8
    if testBit b 7 then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return (val .|. ((fromIntegral b) `shiftL` n))

-- This should be used on unsigned Integral types only (not checked)
{-# INLINE putVarWord #-}
putVarWord :: (Integral a, Bits a) => a -> Put
putVarWord i | i < 0x80 = putWord8 (fromIntegral i)
             | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarWord (i `shiftR` 7)

-}

{-
{-# INLINE wordMS16 #-}
wordMS16 :: P.BoundedPrim Word64
wordMS16 = fixed1 $ PI.fixedPrim 2 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)

{-# INLINE wordMS24 #-}
wordMS24 :: P.BoundedPrim Word64
wordMS24 = fixed1 $ PI.fixedPrim 3 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)

{-# INLINE wordMS64 #-}
wordMS64 :: P.BoundedPrim Word64
wordMS64 = fixed1 $ PI.fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral w :: Word8)
-}
{-
{-# INLINE wordMP #-}
wordMP :: P.BoundedPrim Word
wordMP =
    condB (<= 0x7f)       (   fixed1      (fromIntegral >$< P.word8   )) $
    condB (<= 0xff)       (preFixed1 0xcc (fromIntegral >$< P.word8   )) $
    condB (<= 0xffff)     (preFixed1 0xcd (fromIntegral >$< P.word16BE)) $
#ifdef ARCH_64bit
    condB (<= 0xffffffff) (preFixed1 0xce (fromIntegral >$< P.word32BE)) $
                          (preFixed1 0xcf (fromIntegral >$< P.word64BE))
#else
                          (preFixed1 0xce (fromIntegral >$< P.word32BE))
#endif


-- BROKEN?
{-# INLINE varWordMP #-}
varWordMP :: P.BoundedPrim Word
varWordMP =
    condB (<= 0x7f) (fixed1 (fromIntegral >$< P.word8 )) $
      (\i -> (fromIntegral (i .&. 0x7F) .|. 0x80,i `shiftR` 7)) >$< (fixed1 P.word8 >*< varWordMP)

{-# INLINE varIntMP #-}
varIntMP :: P.BoundedPrim Int
varIntMP = (fromIntegral . zzEncode64 . fromIntegral) >$< varWordMP
-}
