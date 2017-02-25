{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Flat.Pokes (
    E(..),
    Signal(..),
    Step(..),
    S(..),Prim,
    NumBits,
    availBits,
    bitsToBytes,
    done,
    notEnoughSpace,
    bitEncoderStrict,
    bitEncoderLazy,
    bitEncoderRetry,
    eBitsS,
    eBitsF,
    eFloatF,
    eDoubleF,
    eUTF16S,
    eUTF16F,
    sUTF16,
    eCharF,
    eNaturalF,eNaturalS,eIntegerS,
    eIntegerF,
    eInt64F,
    eInt32F,
    eIntF,
    eInt16F,
    eInt8F,
    eWordF,
    eWord64F,
    eWord32F,
    eWord16F,
    eBytesS,
    eBytesF,
    sBytes,
    eLazyBytesS,
    eLazyBytesF,
    sLazyBytes,
    eShortBytesS,
    eShortBytesF,
    sShortBytes,
    eWord64S,
    eWord32S,
    eWord16S,
    eWord32BES,
    eWord64BES,
    eWord32BEF,
    eWord64BEF,
    eWord8F,
    eFillerS,
    eFillerF,
    eBoolF,
    eBoolS,
    eTrueS,
    eTrueF,
    eFalseS,
    eFalseF,
    varWordF,
    sizeWord,
    numBlks,
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
    --csize,
    module Data.Word,module Data.Int,Natural,SBS.ShortByteString,T.Text
    ) where

import           Control.Monad
import qualified Data.ByteString                as B
import qualified Data.ByteString.Internal       as BS
import qualified Data.ByteString.Lazy           as L
import qualified Data.ByteString.Lazy.Internal  as L
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Primitive.ByteArray
import qualified Data.Text                      as T
import qualified Data.Text.Array                as TA
import qualified Data.Text.Internal             as T
import           Data.Word
import           Data.Int
import           Foreign
import           GHC.Prim (copyByteArrayToAddr#)
import           GHC.Ptr                        (Ptr (..))
import           GHC.Types                      (IO (..), Int (..))
import           System.Endian
import           System.IO.Unsafe
import GHC.Conc hiding (Signal)
import           Data.ZigZag
import           Data.Char
import           Numeric.Natural
import           Data.Binary.FloatCast
-- import Debug.Trace
#include "MachDeps.h"

traceShowId :: a -> a
traceShowId = id

{-# SPECIALIZE INLINE sizeWord :: Word16 -> NumBits #-}
{-# SPECIALIZE INLINE sizeWord :: Word32 -> NumBits #-}
{-# SPECIALIZE INLINE sizeWord :: Word64 -> NumBits #-}
{-# SPECIALIZE INLINE sizeWord :: Word -> NumBits #-}

-- Strict encoder
bitEncoderStrict :: Int -> Prim -> BS.ByteString
bitEncoderStrict !numBits op =
  let bufSize = bitsToBytes numBits
  in fst $ unsafeCreateUptoN' bufSize $
        \ptr -> do
          (S ptr' 0 0) <- op (S ptr 0 0)
          return (ptr' `minusPtr` ptr,())

{-# NOINLINE historicSizes #-}
-- Store observed sizes of encodings
historicSizes :: TVar Int
-- historicSizes :: TVar (M.Map TypeRep Int)
-- historicSizes = unsafePerformIO $ newTVarIO M.empty
historicSizes = unsafePerformIO $ newTVarIO 1

-- Try to encode with a guessed size, if it fails restart enconding from start with a longer size
bitEncoderRetry :: Show e => Encoder e -> e -> L.ByteString
bitEncoderRetry enc e = unsafePerformIO $ do
  guessedSize <- atomically $ readTVar historicSizes
  let bs = bitEncoderRetry_ guessedSize enc e
  let lastSize = fromIntegral $ L.length bs
  atomically $ writeTVar historicSizes lastSize
  -- print $ unwords ["guessedSize",show guessedSize,"actualSize",show lastSize,if lastSize > guessedSize then "MISS" else "HIT"]
  return bs

bitEncoderRetry_ :: Show e => Int -> Encoder e -> e -> L.ByteString
bitEncoderRetry_ initialBufSize encoder encoding | initialBufSize > 0 = enc [] initialBufSize 0 0 encoding
                                                 | otherwise = error "bitEncoderRetry_: initialBuffer size must be positive"
  where
   enc bufs bufSize w u e = do
     let (bs,signal) = unsafeCreateUptoN' bufSize $ \ptr -> do
           --print $ unwords ["bufsize",show bufSize]
           !s <- encoder (E (ptr `plusPtr` bufSize) (S ptr w u)) e
           return (nextPtr (getState s) `minusPtr` ptr,s)

     case traceShowId signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       -- Retry n -> bitEncoderRetry (nextBufSize bufSize (bitsToBytes n)) encoder encoding
       -- NotEnoughSpace _ n _ -> bitEncoderRetry (nextBufSize bufSize (bitsToBytes n)) encoder encoding
       NotEnoughSpace _ _ _ -> enc [] (bufSize*2) 0 0 encoding
       -- BUG: wasted space, also state cannot be returned so usedBits must be == 0
       --InsertBytes ibs e' -> enc (ibs:bs:bufs) (nextBufSize initialBufSize bufSize) 0 0 e'
       o -> error $ unwords ["bitEncoderRetry: Unexpected value",show o]

-- Note: wastes some space
-- TODO: avoid empty or very short chunks
bitEncoderLazy :: Show e => Int -> Encoder e -> e -> L.ByteString
bitEncoderLazy initialBufSize encoder encoding | initialBufSize > 0 = enc [] initialBufSize 0 0 encoding
                                               | otherwise = error "bitEncoderLazy: initialBuffer size must be positive"
  where
   enc bufs bufSize w u e = do
     let (bs,signal) = unsafeCreateUptoN' bufSize $ \ptr -> do
           --print $ unwords ["bufsize",show bufSize]
           !s <- encoder (E (ptr `plusPtr` bufSize) (S ptr w u)) e
           return (nextPtr (getState s) `minusPtr` ptr,s)

     case traceShowId signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       NotEnoughSpace s' n e' -> enc (bs:bufs) (nextBufSize bufSize (bitsToBytes n)) (currByte s') (usedBits s') e'
       -- BUG: wasted space, also state cannot be returned so usedBits must be == 0
       -- InsertBytes ibs e' -> enc (ibs:bs:bufs) (nextBufSize initialBufSize bufSize) 0 0 e'
       o -> error $ unwords ["bitEncoderLazy: Unexpected value",show o]

nextBufSize :: Integral a => a -> a -> a
nextBufSize currentSize requestedSize = max requestedSize $ if currentSize < 65536 then currentSize *2 else currentSize * 3 `div` 2

type Encoder e = E -> e -> IO (Signal e)

data Signal e = Done {-# UNPACK #-} !S
              | NotEnoughSpace {-# UNPACK #-} !S {-# UNPACK #-} !Int e
              | InsertByteString {-# UNPACK #-} !S BS.ByteString e
  deriving Show

done :: forall e m. Monad m => S -> m (Signal e)
done = return . Done

notEnoughSpace :: Monad m => S -> Int -> e -> m (Signal e)
notEnoughSpace s n e = return (NotEnoughSpace s n e)

getState :: Signal t -> S
getState (Done s)               = s
getState (NotEnoughSpace s _ _) = s
getState (InsertByteString s _ _) = s

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
  fp <- BS.mallocByteString l
  (l', res) <- withForeignPtr fp $ \p -> f p
  --print (unwords ["Buffer allocated:",show l,"bytes, used:",show l',"bytes"])
  when (l'> l) $ error (unwords ["Buffer overflow, allocated:",show l,"bytes, used:",show l',"bytes"])
  return (BS.PS fp 0 l', res) -- , minusPtr l')
{-# INLINE createUptoN' #-}

--data E = E { lastPtr :: {-# UNPACK #-} !(Ptr Word8), currState :: {-# UNPACK #-} !S }

data E = E { lastPtr :: {-# UNPACK #-} !(Ptr Word8), currState :: !S }

{-# INLINE availBytes #-}
availBytes :: E -> Int
availBytes e = lastPtr e `minusPtr` nextPtr (currState e)

{-# INLINE availBits #-}
availBits :: E -> Int
-- We consider only the memory space
availBits = (8*) . availBytes
--availBits e = 8* (lastPtr e `minusPtr` nextPtr (currState e)) - usedBits (currState e)
-- availBits (E lastPtr s) = 8* (lastPtr `minusPtr` nextPtr s) - usedBits s

--{-# INLINE availBits_ #-}
--availBits_ :: Ptr Word8 -> S -> Int
--availBits_ lastPtr s = 8* (lastPtr `minusPtr` nextPtr s) - usedBits s

{-# INLINE bitsToBytes #-}
bitsToBytes :: Int -> Int
bitsToBytes = numBlks 8

{-# INLINE numBlks #-}
numBlks :: Integral t => t -> t -> t
numBlks blkSize bits = let (d,m) = bits `divMod` blkSize
                       in d + (if m==0 then 0 else 1)

{-# INLINE hasBytes #-}
hasBytes :: E -> Int -> Bool
hasBytes e n = nextPtr (currState e) `plusPtr` n <= lastPtr e

data S =
       S
         { nextPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , currByte :: {-# UNPACK #-} !Word8
         , usedBits :: {-# UNPACK #-} !NumBits
         } deriving Show

data Step = Step { stepSize :: !NumBits, stepF :: Prim }

type Prim = S -> IO S

-- FIX: Should be Int64 or Word64
type NumBits = Int

instance Show Step where show (Step n _) = unwords ["Step",show n]

sFloat :: Float -> NumBits
sFloat _ = 32

eFloatF :: Float -> Prim
eFloatF = eWord32BEF . floatToWord

sDouble :: Double -> NumBits
sDouble _ = 64

eDoubleF :: Double -> Prim
eDoubleF = eWord64BEF . doubleToWord

sChar :: Char -> NumBits
sChar  = sWord32  . fromIntegral . ord

eCharF :: Char -> Prim
eCharF = eWord32F . fromIntegral . ord

-- eWordS :: Word -> Step
-- eWordS w = Step ()

#if WORD_SIZE_IN_BITS == 64

eWordF :: Word -> Prim
eWordF = eWord64F . (fromIntegral :: Word -> Word64)
eIntF :: Int -> Prim
eIntF = eInt64F . (fromIntegral :: Int -> Int64)
sWord :: Word -> NumBits
sWord = sWord64 . (fromIntegral :: Word -> Word64)
sInt :: Int -> NumBits
sInt = sInt64 . (fromIntegral :: Int -> Int64)

#elif WORD_SIZE_IN_BITS == 32

eWordF :: Word -> Prim
eWordF = eWord32F . (fromIntegral :: Word -> Word32)
eIntF :: Int -> Prim
eIntF = eInt32F . (fromIntegral :: Int -> Int32)
sWord :: Word -> NumBits
sWord = sWord32 . (fromIntegral :: Word -> Word32)
sInt :: Int -> NumBits
sInt = sInt32 . (fromIntegral :: Int -> Int32)

#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

--{-# INLINE eListElemS #-}
--eListElemS :: Step -> Step
--eListElemS (Step n f) = Step (n+1) (eTrueF >=> f)

{-# INLINE eWord64S #-}
eWord64S :: Word64 -> Step
eWord64S t = Step (10*8) (varWordF t)

{-# INLINE eWord32S #-}
eWord32S :: Word32 -> Step
eWord32S t = Step (5*8) (varWordF t)

{-# INLINE eWord16S #-}
eWord16S :: Word16 -> Step
eWord16S t = Step (3*8) (varWordF t)

{-# INLINE eWord64LE #-}
eWord64LE :: Word64 -> Step
eWord64LE t = Step 64 (eWord64E toLE64 t)

{-# INLINE eWord32LE #-}
eWord32LE :: Word32 -> Step
eWord32LE t = Step 32 (eWord32E toLE32 t)

{-# INLINE eWord64BES #-}
eWord64BES :: Word64 -> Step
eWord64BES t = Step 64 (eWord64BEF t)

{-# INLINE eWord64BEF #-}
eWord64BEF :: Word64 -> Prim
eWord64BEF = eWord64E toBE64

{-# INLINE eWord32BES #-}
eWord32BES :: Word32 -> Step
eWord32BES t = Step 32 (eWord32BEF t)

eWord32BEF :: Word32 -> Prim
eWord32BEF = eWord32E toBE32

sInt16 :: Int16 -> NumBits
sInt16 = sWord16 . zzEncode16

sInt32 :: Int32 -> NumBits
sInt32 = sWord32 . zzEncode32

sInt64 :: Int64 -> NumBits
sInt64 = sWord64 . zzEncode64

eInt8F :: Int8 -> Prim
eInt8F = eWord8F . zzEncode8

eInt16F :: Int16 -> Prim
eInt16F = eWord16F . zzEncode16

eInt32F :: Int32 -> Prim
eInt32F = eWord32F . zzEncode32

eInt64F :: Int64 -> Prim
eInt64F = eWord64F . zzEncode64

sWord16 :: Word16 -> NumBits
sWord16 = sizeWord

sWord32 :: Word32 -> NumBits
sWord32 = sizeWord

sWord64 :: Word64 -> NumBits
sWord64 = sizeWord

{-# INLINE eWord8S #-}
eWord8S :: Word8 -> Step
eWord8S t = Step 8 (eWord8F t)

sWord8 :: Word8 -> Int
sWord8 _ = 8

sInt8 :: Int8 -> Int
sInt8 _ = 8

{-# INLINE eBitsS #-}
eBitsS :: NumBits -> Word8 -> Step
eBitsS n t = Step n $ eBitsF n t

{-# INLINE eFillerS #-}
eFillerS :: Step
eFillerS = Step 8 eFillerF

{-# INLINE eBoolS #-}
eBoolS :: Bool -> Step
eBoolS = Step 1 . eBoolF

{-# INLINE eTrueS #-}
eTrueS :: Step
eTrueS = Step 1 eTrueF

{-# INLINE eFalseS #-}
eFalseS :: Step
eFalseS = Step 1 eFalseF

eIntegerS :: Integer -> Step
eIntegerS = eIntegralS . zzEncodeInteger

{-# INLINE eIntegerS #-}
eIntegerF :: Integer -> Prim
eIntegerF = eIntegralF . zzEncodeInteger

sInteger :: Integer -> NumBits
sInteger = sIntegral . zzEncodeInteger

eNaturalS :: Natural -> Step
eNaturalS = eIntegralS . toInteger

{-# INLINE eNaturalF #-}
eNaturalF :: Natural -> Prim
eNaturalF = eIntegralF . toInteger

sNatural :: Natural -> NumBits
sNatural = sIntegral . toInteger

{-# INLINE eIntegralS #-}
eIntegralS :: Integer -> Step
eIntegralS t = let vs = w7l $ t
               in Step (length vs*8) (eIntegralW vs)

-- BAD: duplication of work
{-# INLINE sIntegral #-}
sIntegral :: (Bits t, Integral t) => t -> Int
sIntegral t = let vs = w7l t
              in length vs*8

{-# INLINE eIntegralF #-}
eIntegralF :: (Bits t, Integral t) => t -> Prim
eIntegralF t = let vs = w7l t
               in eIntegralW vs

-- Encode as data NEList = Elem Word7 | Cons Word7 List
{-# INLINE eIntegralW #-}
eIntegralW :: [Word8] -> Prim
eIntegralW vs s@(S op _ o) | o == 0 = foldM pokeWord' op vs >>= \op' -> return (S op' 0 0)
                           | otherwise = foldM (flip eWord8F) s vs

w7l :: (Bits t, Integral t) => t -> [Word8]
w7l t = let l  = low7 t
            t' = t `shiftR` 7
        in if t' == 0
           then [l]
           else w7 l : w7l t'

{-# INLINE low7 #-}
low7 :: (Integral a) => a -> Word8
low7 t = fromIntegral t .&. 0x7F

{-# INLINE w7 #-}
--lowByte :: (Bits t, Num t) => t -> Word8
w7 :: Word8 -> Word8
w7 l = l .|. 0x80

{-# INLINE eWord16F #-}
eWord16F :: Word16 -> Prim
eWord16F = varWordF

{-# INLINE eWord32F #-}
eWord32F :: Word32 -> Prim
eWord32F = varWordF

{-# INLINE eWord64F #-}
eWord64F :: Word64 -> Prim
eWord64F = varWordF

{-# INLINE varWordF #-}
varWordF :: (Bits t, Integral t) => t -> Prim
varWordF t s@(S _ _ o) | o == 0 = varWord pokeByteAligned t s
                       | otherwise = varWord pokeByteUnaligned t s

{-# INLINE varWord #-}
varWord :: (Bits t, Integral t) => (Word8 -> Prim) -> t -> Prim
varWord writeByte t s
  | t < 128 = writeByte (fromIntegral t) s
  -- TODO: optimise, using a single Write16
  | t < 16384 = writeByte (fromIntegral t .|. 0x80) s >>= writeByte (fromIntegral (t `shiftR` 7) .&. 0x7F)
  | t < 2097152 = writeByte (fromIntegral t .|. 0x80) s >>= writeByte (fromIntegral (t `shiftR` 7) .|. 0x80) >>= writeByte (fromIntegral (t `shiftR` 14) .&. 0x7F)
  | otherwise = go t s
  where go v st = let l  = low7 v
                      v' = v `shiftR` 7
                  in if v' == 0
                     then writeByte l st
                     else writeByte (l .|. 0x80) st >>= go v'

{-# INLINE sizeWord #-}
sizeWord :: (Ord a, Num a, Num t) => a -> t
sizeWord w | w < 128 = 8
           | w < 16384 = 16
           | w < 2097152 = 24
           | w < 268435456 = 32
           | w < 34359738368 = 40
           | w < 4398046511104 = 48
           | w < 562949953421312 = 56
           | w < 72057594037927936 = 64
           | w < 9223372036854775808 = 72
           | otherwise = 80

-- Array Char
-- A unicode char can always be encoded as 3 bytes (Word32)
-- eString s = Step (length s*8*3) (eUTF16F . T.pack $ s)
-- eArrayChar = eUTF16S . T.pack
-- eArrayChar s = Step (length s*8*3) (eUTF16F . T.pack $ s) -- eUTF16 . T.pack

--arrayBits = (8+) . bsBits_

{-# INLINE blobBits #-}
blobBits :: Int -> NumBits
blobBits numBytes = 16 -- initial filler + final 0
                    + blksBits numBytes

{-# INLINE blkBitsBS #-}
blkBitsBS :: BS.ByteString -> NumBits
blkBitsBS = blksBits . B.length

{-# INLINE blksBits #-}
blksBits :: Int -> NumBits
blksBits numBytes = 8*(numBytes + numBlks 255 numBytes)

eUTF16S :: T.Text -> Step
eUTF16S !t = Step (sUTF16 t) (eUTF16F t)

{-# INLINE sUTF16 #-}
sUTF16 :: T.Text -> NumBits
sUTF16 = blobBits . textBytes

eUTF16F :: T.Text -> Prim
eUTF16F t = eFillerF >=> eUTF16F_ t
  where
    eUTF16F_ !(T.Text (TA.Array array) w16Off w16Len) s = writeArray array (2 * w16Off) (2 * w16Len) (nextPtr s)

-- We are not interested in the number of unicode chars (returned by T.length, an O(n) operation)
-- just the number of bytes
-- > T.length (T.pack "\x1F600")
-- 1
-- > textBytes (T.pack "\x1F600")
-- 4
textBytes :: T.Text -> Int
textBytes !(T.Text _ _ w16Len) = w16Len * 2

eBytesS :: B.ByteString -> Step
eBytesS bs = Step (sBytes bs) (eBytesF bs)

sBytes :: B.ByteString -> NumBits
sBytes = blobBits . B.length

eLazyBytesS :: L.ByteString -> Step
eLazyBytesS bs = Step (sLazyBytes bs) (eLazyBytesF bs)

sLazyBytes :: L.ByteString -> NumBits
sLazyBytes bs = 16 + L.foldrChunks (\b l -> blkBitsBS b + l) 0 bs

eLazyBytesF :: L.ByteString -> Prim
eLazyBytesF bs = eFillerF >=> \s -> write bs (nextPtr s)
  where
    -- Single copy
    write lbs op = do
     case lbs of
       L.Chunk h t -> writeBS h op >>= write t
       L.Empty     -> pokeWord op 0

{-# INLINE eShortBytesS #-}
eShortBytesS :: SBS.ShortByteString -> Step
eShortBytesS bs = Step (sShortBytes bs) (eShortBytesF bs)

{-# INLINE sShortBytes #-}
sShortBytes :: SBS.ShortByteString -> NumBits
sShortBytes = blobBits . SBS.length

{-# INLINE eShortBytesF #-}
eShortBytesF :: SBS.ShortByteString -> Prim
eShortBytesF bs = eFillerF >=> eShortBytesF_ bs

eShortBytesF_ :: SBS.ShortByteString -> Prim
eShortBytesF_ bs@(SBS.SBS arr) = \(S op _ 0) -> writeArray arr 0 (SBS.length bs) op

-- data Array a = Array0 | Array1 a ... | Array255 ...
writeArray :: ByteArray# -> Int -> Int -> Ptr Word8 -> IO S
writeArray arr soff slen sop = do
  op' <- go soff slen sop
  pokeWord op' 0
  where
    go !off !len !op
      | len == 0 = return op
      | otherwise =
          let l = min 255 len
          in pokeWord' op (fromIntegral l) >>=
             pokeByteArray arr off l >>=
             go (off+l) (len - l)

eBytesF :: B.ByteString -> Prim
eBytesF bs = eFillerF >=> eBytesF_
  where
    eBytesF_ s = do
      op' <- writeBS bs (nextPtr s)
      pokeWord op' 0

writeBS :: B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
writeBS bs op -- @(BS.PS foreignPointer sourceOffset sourceLength) op
  | B.length bs == 0 = return op
  | otherwise =
    let (h, t) = B.splitAt 255 bs
    in pokeWord' op (fromIntegral $ B.length h :: Word8) >>= pokeByteString h >>= writeBS t

    -- 2X slower (why?)
    -- withForeignPtr foreignPointer goS
    --   where
    --     goS sourcePointer = go op (sourcePointer `plusPtr` sourceOffset) sourceLength
    --       where
    --         go !op !off !len | len == 0 = return op
    --                          | otherwise = do
    --                           let l = min 255 len
    --                           op' <- pokeWord' op (fromIntegral l)
    --                           BS.memcpy op' off l
    --                           go (op' `plusPtr` l) (off `plusPtr` l) (len-l)


pokeByteString :: B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeByteString (BS.PS foreignPointer sourceOffset sourceLength) destPointer = do
    withForeignPtr foreignPointer $ \sourcePointer ->
      BS.memcpy destPointer (sourcePointer `plusPtr` sourceOffset) sourceLength
    return (destPointer `plusPtr` sourceLength)

pokeByteArray :: ByteArray# -> Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
pokeByteArray sourceArr sourceOffset len dest = do
        copyByteArrayToAddr sourceArr sourceOffset dest len
        let !dest' = dest `plusPtr` len
        return dest'
{-# INLINE pokeByteArray #-}

-- | Wrapper around @copyByteArrayToAddr#@ primop.
-- Copied from the store-core package
copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))
{-# INLINE copyByteArrayToAddr  #-}

{-# INLINE eWord8F #-}
eWord8F :: Word8 -> Prim
eWord8F t s@(S op _ o) | o==0 = pokeWord op t
                       | otherwise = pokeByteUnaligned t s

{-# INLINE eWord32E #-}
eWord32E :: (Word32 -> Word32) -> Word32 -> Prim
eWord32E conv t (S op w o) | o==0 = pokeW conv op t >> skipBytes op 4
                           | otherwise = pokeW conv op (fromIntegral w `shiftL` 24 .|. t `shiftR` o) >> return (S (plusPtr op 4) (fromIntegral t `shiftL` (8-o)) o)

{-# INLINE eWord64E #-}
eWord64E :: (Word64 -> Word64) -> Word64 -> Prim
eWord64E conv t (S op w o) | o==0 = pokeW conv op t >> skipBytes op 8
                           | otherwise = pokeW conv op (fromIntegral w `shiftL` 56 .|. t `shiftR` o) >> return (S (plusPtr op 8) (fromIntegral t `shiftL` (8-o)) o)

--poke64BE f op t = poke (castPtr op) (toBE64 t)

-- {-# INLINE poke32BE #-}
-- {-# INLINE poke64BE #-}
-- poke32BE f op t = poke (castPtr op) (toBE32 t)
-- poke64BE f op t = poke (castPtr op) (toBE64 t)

-- {-# INLINE poke32LE #-}
-- {-# INLINE poke64LE #-}
-- poke32LE op t = poke (castPtr op) (toLE32 t)
-- poke64LE op t = poke (castPtr op) (toLE64 t)

{-
Example:
Before:
n = 6
t = 00.101011
o = 3
w = 111.00000

After:
[ptr] = w(111)t(10101)
w' = t(1)0000000
o'= 1

o'=3+6=9
f = 8-9 = -1
o'' = 1
8-o''=7

if n=8,o=3:
o'=11
f=8-11=-3
o''=3
8-o''=5
-}
{-# INLINE eBitsF #-}
eBitsF :: NumBits -> Word8 -> Prim
eBitsF 1 0 = eFalseF
eBitsF 1 1 = eTrueF
eBitsF 2 0 = eFalseF >=> eFalseF
eBitsF 2 1 = eFalseF >=> eTrueF
eBitsF 2 2 = eTrueF >=> eFalseF
eBitsF 2 3 = eTrueF >=> eTrueF
eBitsF n t = \(S op w o) ->
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
  in if | f > 0  ->  return $ S op (w .|. (t `shiftL` f)) o'
        | f == 0 ->  pokeWord op (w .|. t)
        | otherwise -> let o'' = -f
                       in poke op (w .|. (t `shiftR` o'')) >> return (S (plusPtr op 1) (t `shiftL` (8-o'')) o'')

{-# INLINE eBoolF #-}
eBoolF :: Bool -> Prim
eBoolF False = eFalseF
eBoolF True  = eTrueF

{-# INLINE eTrueF #-}
eTrueF :: Prim
eTrueF (S op w o) | o == 7 = pokeWord op (w .|. 1)
                  | otherwise = return (S op (setBit w (7-o)) (o+1))

{-# INLINE eFalseF #-}
eFalseF :: Prim
eFalseF (S op w o) | o == 7 = pokeWord op w
                   | otherwise = return (S op w (o+1))

{-# INLINE eFillerF #-}
eFillerF :: Prim
eFillerF (S op w _) = pokeWord op (w .|. 1)

-- {-# INLINE poke16 #-}
-- TODO TEST
-- poke16 :: Word16 -> Prim
-- poke16 t (S op w o) | o == 0 = poke op w >> skipBytes op 2

{-# INLINE pokeByteUnaligned #-}
pokeByteUnaligned :: Word8 -> Prim
pokeByteUnaligned t (S op w o) = poke op (w .|. (t `shiftR` o)) >> return (S (plusPtr op 1) (t `shiftL` (8-o)) o)

{-# INLINE pokeByteAligned #-}
pokeByteAligned :: Word8 -> Prim
pokeByteAligned t (S op _ _) = pokeWord op t

{-# INLINE pokeWord #-}
pokeWord :: Storable a => Ptr a -> a -> IO S
pokeWord op w = poke op w >> skipByte op

{-# INLINE pokeWord' #-}
pokeWord' :: Storable a => Ptr a -> a -> IO (Ptr b)
pokeWord' op w = poke op w >> return (plusPtr op 1)

{-# INLINE pokeW #-}
pokeW :: Storable a => (t -> a) -> Ptr a1 -> t -> IO ()
pokeW conv op t = poke (castPtr op) (conv t)

{-# INLINE skipByte #-}
skipByte :: Monad m => Ptr a -> m S
skipByte op = return (S (plusPtr op 1) 0 0)

{-# INLINE skipBytes #-}
skipBytes :: Monad m => Ptr a -> Int -> m S
skipBytes op n = return (S (plusPtr op n) 0 0)

--{-# INLINE nextByteW #-}
--nextByteW op w = return (S (plusPtr op 1) 0 0)
