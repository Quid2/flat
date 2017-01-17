{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}
module Data.Flat.Pokes where

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
-- import Data.Text.Internal.Unsafe.Shift
import           Data.Word
import           Foreign -- (Storable,Bits(..))
import           Foreign.Ptr
import           GHC.Prim                       (RealWorld,
                                                 copyAddrToByteArray#,
                                                 copyByteArrayToAddr#,
                                                 unsafeCoerce#)
import           GHC.Ptr                        (Ptr (..))
import           GHC.Types                      (IO (..), Int (..))
import           System.Endian
import           System.IO.Unsafe

bitEncoderStrict :: Int -> (S -> IO S) -> BS.ByteString
bitEncoderStrict numBits op =
  let bufSize = bitsToBytes numBits
  in fst $ unsafeCreateUptoN' bufSize $
        \ptr -> do
          (S ptr' 0 0) <- op (S ptr 0 0)
          return (ptr' `minusPtr` ptr,())

-- Note: wastes some space
bitEncoderLazy :: Int -> Encoder e -> e -> L.ByteString
bitEncoderLazy initialBufSize encoder encoding | initialBufSize > 0 = enc [] initialBufSize 0 0 encoding
 where
   enc bufs bufSize w u e = do
     let (bs,signal) = unsafeCreateUptoN' bufSize $ \ptr -> do
           !s <- encoder (E (ptr `plusPtr` bufSize) (S ptr w u)) e
           return (nextPtr (getState s) `minusPtr` ptr,s)

     case signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       NotEnoughSpace s' n e' -> enc (bs:bufs) (nextBufSize n bufSize) (currByte s') (usedBits s') e'
       -- BUG: wasted space, also state cannot be returned so usedBits must be == 0
       InsertBytes ibs e' -> enc (ibs:bs:bufs) (nextBufSize initialBufSize bufSize) 0 0 e'

nextBufSize currentSize requestedSize = max requestedSize $ if currentSize < 65536 then currentSize *2 else currentSize * 3 `div` 2

type Encoder e = E -> e -> IO (Signal e)

data Signal e = Done {-# UNPACK #-} !S
              | NotEnoughSpace
                {-# UNPACK #-} !S
                {-# UNPACK #-} !Int
                e
              | InsertBytes
                BS.ByteString
                e

done :: forall e m. Monad m => S -> m (Signal e)
done = return . Done
notEnoughSpace s n e = return (NotEnoughSpace s n e)

getState (Done s)               = s
getState (NotEnoughSpace s _ _) = s

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

data E = E { lastPtr :: {-# UNPACK #-} !(Ptr Word8), currState :: S }

{-# INLINE availBytes #-}
availBytes e = lastPtr e `minusPtr` nextPtr (currState e)

{-# INLINE bitsToBytes #-}
bitsToBytes = numBlks 8

{-# INLINE numBlks #-}
numBlks blkSize bits = let (d,m) = bits `divMod` blkSize
                       in d + (if m==0 then 0 else 1)

{-# INLINE hasBytes #-}
hasBytes e n = nextPtr (currState e) `plusPtr` n <= lastPtr e

data S =
       S
         { nextPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , currByte :: {-# UNPACK #-} !Word8
         , usedBits :: {-# UNPACK #-} !Int
         }

data Step = Step { stepSize :: !Int, stepF :: S -> IO S }

instance Show Step where show (Step n _) = unwords ["Step",show n]


{-# INLINE eUnsigned #-}
eUnsigned :: Integer -> Step
eUnsigned t = let vs = w7l t
              in Step (length vs*8) (eUnsignedF vs)

{-# INLINE eUnsigned64 #-}
eUnsigned64 :: Word64 -> Step
eUnsigned64 t = Step (10*8) (varWordF t)

{-# INLINE eUnsigned32 #-}
eUnsigned32 :: Word32 -> Step
eUnsigned32 t = Step (5*8) (varWordF t)

{-# INLINE eUnsigned16 #-}
eUnsigned16 :: Word16 -> Step
eUnsigned16 t = Step (3*8) (varWordF t)

{-# INLINE eWord64LE #-}
eWord64LE :: Word64 -> Step
eWord64LE t = Step 64 (eWord64F toLE64 t)

{-# INLINE eWord32LE #-}
eWord32LE :: Word32 -> Step
eWord32LE t = Step 32 (eWord32F toLE32 t)

{-# INLINE eWord64BE #-}
eWord64BE :: Word64 -> Step
eWord64BE t = Step 64 (eWord64F toBE64 t)

{-# INLINE eWord32BE #-}
eWord32BE :: Word32 -> Step
eWord32BE t = Step 32 (eWord32F toBE32 t)

{-# INLINE eWord8 #-}
eWord8 :: Word8 -> Step
eWord8 t = Step 8 (eWord8F t)

{-# INLINE eBits #-}
eBits :: Int -> Word8 -> Step
eBits n t = Step n $ eBitsF n t

{-# INLINE eFiller #-}
eFiller = Step 8 eFillerF

{-# INLINE eBool #-}
eBool :: Bool -> Step
eBool = Step 1 . eBoolF

{-# INLINE eTrue #-}
eTrue = Step 1 eTrueF

{-# INLINE eFalse #-}
eFalse = Step 1 eFalseF

-- Encode as data NEList = Elem Word7 | Cons Word7 List
{-# INLINE eUnsignedF #-}
eUnsignedF :: [Word8] -> S -> IO S
eUnsignedF vs s@(S op w o) | o == 0 = foldM pokeWord' op vs >>= \op' -> return (S op' 0 0)
                           | otherwise = foldM (flip eWord8F) s vs

w7l :: (Bits t, Integral t) => t -> [Word8]
w7l t = let l  = fromIntegral t .&. 0x7F
            t' = t `shiftR` 7
        in if t' == 0
           then [l]
           else (l .|. 0x80) : w7l t'

{-# INLINE varWordF #-}
--varWordF :: (Bits t, Integral t) => t -> S -> IO S
varWordF t s@(S op w o) | o == 0 = varWord pokeByteAligned t s
                        | otherwise = varWord pokeByteUnaligned t s

{-# INLINE varWord #-}
-- varWord :: (Bits t, Integral t) => (Word8 -> S -> IO S) -> t -> S -> IO S
varWord writeByte t s | t < 128 = writeByte (fromIntegral t) s
                      | t < 16384 = writeByte (fromIntegral t .&. 0x7F  .|. 0x80) s >>= writeByte (fromIntegral (t `shiftR` 7))
                      | otherwise = go t s
  where go t s = let l  = fromIntegral t .&. 0x7F
                     t' = t `shiftR` 7
                 in if t' == 0
                    then writeByte l s
                    else writeByte (l .|. 0x80) s >>= go t'

{-# INLINE arrayBits #-}
arrayBits = (8+) .arrayBits_

arrayBits_ bs = arrayBits__ (B.length bs)

arrayBitsL l = 8+arrayBits__ l

arrayBits__ l = 8*(l + numBlks 255 l)

-- Array Char
-- A unicode char can always be encoded as 3 bytes (Word32)
-- eString s = Step (length s*8*3) (eUTF16F . T.pack $ s)
eArrayChar = eUTF16 . T.pack
-- eArrayChar s = Step (length s*8*3) (eUTF16F . T.pack $ s) -- eUTF16 . T.pack

-- eText = eFiller >>= eUTF16 t

eUTF16 !t = Step (arrayBitsL (textBytes t)) (eUTF16F t)

-- We are not interested in the number of unicode chars (returned by T.length, an O(n) operation)
-- just the number of bytes
-- > T.length (T.pack "\x1F600")
-- 1
-- > textBytes (T.pack "\x1F600")
-- 4
textBytes !(T.Text _ _ w16Len) = w16Len * 2

eUTF16F !(T.Text (TA.Array array) w16Off w16Len) (S op w o) | o == 0 = writeArray array (2 * w16Off) (2 * w16Len) op
                                                            | otherwise = error "Encoder doesn't support unaligned Text."

-- Single copy
eLazyBytes bs = Step (L.foldrChunks (\b l -> arrayBits_ b + l) 8 bs) (eLazyBytesF bs)

eBytes bs = Step (arrayBits bs) (eBytesF bs)

eShortBytes bs = Step (arrayBitsL (SBS.length bs)) (eShortBytesF bs)

eShortBytesF :: SBS.ShortByteString -> S -> IO S
eShortBytesF bs (S op w o) | o == 0 = writeSBS bs op
                           | otherwise = error "Encoder doesn't support unaligned bytestrings."

writeSBS bs@(SBS.SBS arr) = writeArray arr 0 (SBS.length bs)

writeArray arr off len op = do
  op' <- go off len op
  pokeWord op' 0
  where
    go !off !len !op
      | len == 0 = return op
      | otherwise =
          let l = min 255 len
          in pokeWord' op (fromIntegral l) >>=
             pokeByteArray arr off l >>=
             go (off+l) (len - l)


eLazyBytesF :: L.ByteString -> S -> IO S
eLazyBytesF bs (S op _ o) | o == 0 =  write bs op
                          | otherwise = error "Encoder doesn't support unaligned bytestrings."
 where
   write lbs op = do
     case lbs of
       L.Chunk h t -> writeBS_ h op >>= write t
       L.Empty     -> pokeWord op 0

eBytesF :: B.ByteString -> S -> IO S
eBytesF bs (S op w o) | o == 0 = writeBS bs op
                      | otherwise = error "Encoder doesn't support unaligned bytestrings."


-- Encode as Array Word8
writeBS bs op = do
  op' <- writeBS_ bs op
  pokeWord op' 0

writeBS_ bs@(BS.PS foreignPointer sourceOffset sourceLength) op
  | B.length bs == 0 = return op
  | otherwise =
    let (h, t) = B.splitAt 255 bs
    in pokeWord' op (fromIntegral $ B.length h :: Word8) >>= pokeByteString h >>= writeBS_ t

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
eWord8F :: Word8 -> S -> IO S
eWord8F t s@(S op w o) | o==0 = pokeWord op t
                       | otherwise = pokeByteUnaligned t s

{-# INLINE eWord32F #-}
eWord32F :: (Word32 -> Word32) -> Word32 -> S -> IO S
eWord32F conv t (S op w o) | o==0 = pokeW conv op t >> skipBytes op 4
                           | otherwise = pokeW conv op (fromIntegral w `shiftL` 24 .|. t `shiftR` o) >> return (S (plusPtr op 4) (fromIntegral t `shiftL` (8-o)) o)

{-# INLINE eWord64F #-}
eWord64F :: (Word64 -> Word64) -> Word64 -> S -> IO S
eWord64F conv t (S op w o) | o==0 = pokeW conv op t >> skipBytes op 8
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
eBitsF :: Int -> Word8 -> S -> IO S
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
eBoolF False = eFalseF
eBoolF True  = eTrueF

{-# INLINE eTrueF #-}
eTrueF (S op w o) | o == 7 = pokeWord op (w .|. 1)
                  | otherwise = return (S op (setBit w (7-o)) (o+1))

{-# INLINE eFalseF #-}
eFalseF (S op w o) | o == 7 = pokeWord op w
                   | otherwise = return (S op w (o+1))

{-# INLINE eFillerF #-}
eFillerF (S op w _) = pokeWord op (w .|. 1)

{-# INLINE pokeByteUnaligned #-}
pokeByteUnaligned t (S op w o) = poke op (w .|. (t `shiftR` o)) >> return (S (plusPtr op 1) (t `shiftL` (8-o)) o)

{-# INLINE pokeByteAligned #-}
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
skipByte op = return (S (plusPtr op 1) 0 0)

{-# INLINE skipBytes #-}
skipBytes op n = return (S (plusPtr op n) 0 0)


--{-# INLINE nextByteW #-}
--nextByteW op w = return (S (plusPtr op 1) 0 0)
