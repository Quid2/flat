{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Parse bits easily. Parsing can be done either in a monadic style, or more
-- efficiently, using the 'Applicative' style.
--
-- For the monadic style, write your parser as a 'BitGet' monad using the
--
--   * 'getBool'
--
--   * 'getWord8'
--
--   * 'getWord16be'
--
--   * 'getWord32be'
--
--   * 'getWord64be'
--
--   * 'getByteString'
--
-- functions and run it with 'runBitGet'.
--
-- For the applicative style, compose the fuctions
--
--   * 'bool'
--
--   * 'word8'
--
--   * 'word16be'
--
--   * 'word32be'
--
--   * 'word64be'
--
--   * 'byteString'
--
-- to make a 'Block'.
-- Use 'block' to turn it into the 'BitGet' monad to be able to run it with
-- 'runBitGet'.
-----------------------------------------------------------------------------

module Data.Binary.Bits.Get
            (
            -- * BitGet monad

            -- $bitget
            BitGet
              --, runGet
            -- ,runGetOrFail
            ,runPartialGet

            -- ** Get bytes
            , getBool
            , getByte
            , getFloat,getDouble
            ,getByteString
            -- , getWord16be
            -- , getWord32be
            -- , getWord64be

            -- * Blocks
            ,Block,block,bool,byte,float,double,byteString
            -- $blocks
            -- , Block
            -- , block

            -- ** Read in Blocks
            --,dBool
            --,dWord8,dWord32,dWord64,dBits,dUnsigned
            --,dropBits
            --, bool
            --, byte
            -- , word16be
            -- , word32be
            -- , word64be

            -- , Data.Binary.Bits.Get.getByteString
            -- , Data.Binary.Bits.Get.getLazyByteString
            -- , Data.Binary.Bits.Get.isEmpty
            -- ,funny,funny2
            ) where

import qualified Data.Binary.Get          as B (Get--, getByteString,
                                                ,getLazyByteString, isEmpty,
                                                runGet, runGetOrFail)
import qualified Data.Binary.Get.Internal as B (ensureN, get, put)

import           Control.Applicative
import           Data.Bits
import           Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
-- 
import           Data.ByteString.Unsafe
import           Data.Word
import           Prelude                  as P

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- import           GHC.Base
-- import           GHC.Word
#endif
import Control.DeepSeq

import           Data.Binary.FloatCast
import           System.Endian
import Data.ByteString.Internal
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr              (plusPtr)
import Foreign.Storable         (Storable(..))
-- import Debug.Trace
traceShowId :: t -> t
traceShowId a = a

-- type Get = BitGet --BitG.Block --

-- $bitget
-- Parse bits using a monad.
--
-- @
--myBitParser :: 'Get' ('Word8', 'Word8')
--myBitParser = 'runGetBit' parse4by4
--
--parse4by4 :: 'BitGet' ('Word8', 'Word8')
--parse4by4 = do
--   bits <- 'getWord8' 4
--   more <- 'getWord8' 4
--   return (bits,more)
-- @

-- $blocks
-- Parse more efficiently in blocks. Each block is read with only one boundry
-- check (checking that there is enough input) as the size of the block can be
-- calculated statically. This is somewhat limiting as you cannot make the
-- parsing depend on the input being parsed.
--
-- @
--data IPV6Header = IPV6Header {
--     ipv6Version :: 'Word8'
--   , ipv6TrafficClass :: 'Word8'
--   , ipv6FlowLabel :: 'Word32
--   , ipv6PayloadLength :: 'Word16'
--   , ipv6NextHeader :: 'Word8'
--   , ipv6HopLimit :: 'Word8'
--   , ipv6SourceAddress :: 'ByteString'
--   , ipv6DestinationAddress :: 'ByteString'
-- }
--
-- ipv6headerblock =
--         IPV6Header '<$>' 'word8' 4
--                    '<*>' 'word8' 8
--                    '<*>' 'word32be' 24
--                    '<*>' 'word16be' 16
--                    '<*>' 'word8' 8
--                    '<*>' 'word8' 8
--                    '<*>' 'byteString' 16
--                    '<*>' 'byteString' 16
--
--ipv6Header :: 'Get' IPV6Header
--ipv6Header = 'runBitGet' ('block' ipv6headerblock)
-- @

data S = S {-# UNPACK #-} !ByteString -- Input
           {-# UNPACK #-} !Int -- Bit offset (0-7)
          deriving (Show)

-- | A block that will be read with only one boundery check. Needs to know the
-- number of bits in advance.
data Block a = Block Int (S -> a)

instance Functor Block where
  fmap f (Block i p) = Block i (\s -> f (p s))

instance Applicative Block where
  pure a = Block 0 (\_ -> a)
  (Block i p) <*> (Block j q) = Block (i+j) (\s -> p s $ q (incS i s))
  (Block i _)  *> (Block j q) = Block (i+j) (q . incS i)
  (Block i p) <*  (Block j _) = Block (i+j) p

-- | Get a block. Will be read with one single boundery check, and
-- therefore requires a statically known number of bits.
-- Build blocks using 'bool', 'word8', 'word16be', 'word32be', 'word64be',
-- 'byteString' and 'Applicative'.
{-# INLINE block #-}
block :: Block a -> BitGet a
-- block (Block i p) =
--   B $ \s -> do
--         let s'@(S bs' _) = incS i s

--         if S.length bs' >= 0
--           then return $! p s'

--           else loadBS i s

--   putState $! (incS i s)
--   return $! p s

block (Block i p) = do
  ensureBits i
  s <- getState
  putState $! (incS i s)
  return $! p s

{-# INLINE incS #-}
incS :: Int -> S -> S
incS 1 (S bs o) =
  if o == 7
  then S (unsafeTail bs) 0
  else S bs (o+1)

incS 8 (S bs o) = S (unsafeTail bs) o

incS 32 (S bs o) = S (unsafeDrop 4 bs) o

incS 64 (S bs o) = S (unsafeDrop 8 bs) o

incS o (S bs n) =
  let !o' = n+o
      !d = o' `shiftR` 3
      !n' = o' .&. 7
  in S (unsafeDrop d bs) n'

-- | Make sure there are at least @n@ bits.
{-# INLINE ensureBits #-}
ensureBits :: Int -> BitGet ()
ensureBits 1 = do
   s@(S bs _) <- getState
   if S.length bs > 0 then return () else loadBS 1 s

ensureBits n = do
  s@(S bs o) <- getState
  if n <= S.length bs * 8 - o
    then return ()
    else loadBS n s

loadBS :: Int -> S -> BitGet ()
loadBS n (S bs o) = do
  let currentBits = S.length bs * 8 - o
  let byteCount = (n - currentBits + 7) `div` 8
  B $ \_ -> do B.ensureN byteCount
               bs' <- B.get
               B.put S.empty
               return (S (bs`append`bs') o, ())

{-# INLINE readBool #-}
readBool :: S -> Bool
-- readBool (S bs n) = testBit (unsafeHead bs) (7-n)
readBool (S bs n) = 0 /= (unsafeHead bs .&. (128 `shiftR` n))

{-# INLINE readByte #-}
readByte :: S -> Word8
readByte (S bs o)
  -- all bits are in the same byte
  | 0 == o = unsafeHead bs

  -- the bits are in two different bytes
  -- msb      msb
  -- xBBBBBBB Bxxxxxxx
  | otherwise = (unsafeHead bs `unsafeShiftL` o) .|. (unsafeIndex bs 1 `unsafeShiftR` (8-o))

{-# INLINE readFloat #-}
readFloat :: S -> Float
readFloat (S bs o) = wordToFloat word32
  where word32 | o == 0 = peekBE
               | otherwise = (peekBE `unsafeShiftL` o) .|.  fromIntegral (unsafeIndex bs 4 `unsafeShiftR` (8-o))
        peekBE = toBE32 $ unsafePeek bs

-- {-# INLINE readDouble #-}
readDouble :: S -> Double
-- readDouble = wordToDouble . readWord64be 64
readDouble (S bs o) = wordToDouble word64
  where word64 | o == 0 = peekBE
               | otherwise = (peekBE `unsafeShiftL` o) .|.  fromIntegral (unsafeIndex bs 8 `unsafeShiftR` (8-o))
        peekBE = toBE64 $ unsafePeek bs

{-# INLINE unsafePeek #-}
unsafePeek :: Storable a => ByteString -> a
unsafePeek (PS x s _) = accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peek (p `plusPtr` s)

readByteString :: Int -> S -> ByteString
readByteString n s@(S bs o)
  -- no offset, easy.
  | o == 0 = unsafeTake n bs
  -- offset. ugg. this is really naive and slow. but also pretty easy :)
  -- | otherwise = S.pack (P.map (readByte) (P.take n (iterate (incS 8) s)))
  | otherwise = error "Unaligned bytestrings are unsupported"

------------------------------------------------------------------------
-- | 'BitGet' is a monad, applicative and a functor. See 'runBitGet'
-- for how to run it.
newtype BitGet a = B { runState :: S -> B.Get (S,a) }

instance Monad BitGet where
  {-# INLINE return #-}
  return x = B $ \s -> return (s,x)

  {-# INLINE (>>=) #-}
  (B f) >>= g = B $ \s -> do (s',a) <- f s
                             runState (g a) s'

  fail str = B $ \(S inp n) -> putBackState inp n >> fail str

instance Functor BitGet where
  {-# INLINE fmap #-}
  --fmap f m = m >>= \a -> return (f a)
  fmap f (B m) = B $ \s -> do (s',a) <- m s
                              return (s',f a)

instance Applicative BitGet where
  {-# INLINE pure #-}
  pure x = return x

  {-# INLINE (<*>) #-}
  fm <*> m = fm >>= \f -> m >>= \v -> return (f v)

-- Is this ok?
instance NFData (BitGet a) where rnf !_ = ()

instance Show (BitGet a) where show _ = "BitGet"

{-
runBitGet m b = case G.runGetOrFail (BitG.runBitGet (BitG.block m)) b of
                   Left (_,_,s) -> Left s
                   Right (_,_,a) -> Right a
-}
runPartialGet :: BitGet b
              -> L.ByteString
              -> Int -- num of msbs to skip
              -> Either String (b
                               ,L.ByteString -- left over bytes
                               ,Int -- decoded bits in first byte
                               )
runPartialGet bg bs n = case B.runGetOrFail (runPartialBitGet n bg) bs of
               Left (_,_,s) -> Left s
               Right (bs,_,(a,n)) -> Right (a,bs,n)

runGet :: BitGet b -> L.ByteString -> b
runGet m = B.runGet (runBitGet m)

runGetOrFail :: BitGet b -> L.ByteString -> Either String b
runGetOrFail m b = case B.runGetOrFail (runBitGet m) b of
               Left (_,_,s) -> Left s
               Right (_,_,a) -> Right a

-- | Run a 'BitGet' within the Binary packages 'Get' monad. If a byte has
-- been partially consumed it will be discarded once 'runBitGet' is finished.
runBitGet :: BitGet a -> B.Get a
runBitGet bg = do
  s <- mkInitState
  ((S str' n),a) <- runState bg s
  putBackState str' n
  return a

mkInitState :: B.Get S
mkInitState = do
  str <- B.get
  B.put S.empty
  return (S str 0)

runPartialBitGet :: Int
                 -> BitGet a
                 -> B.Get (a,Int)
runPartialBitGet n0 bg = do
  (S s0 0) <- mkInitState
  let s = S s0 n0
  (S str' n,a) <- runState bg s
  putBack str'
  return (a,n)

putBack :: S.ByteString -> B.Get ()
putBack bs = do
 remaining <- B.get
 B.put (bs `S.append` remaining)

putBackState :: S.ByteString -> Int -> B.Get ()
putBackState bs n = do
 remaining <- B.get
 B.put (S.drop (if n==0 then 0 else 1) bs `S.append` remaining)

{-# INLINE getState #-}
getState :: BitGet S
getState = B $ \s -> return (s,s)

{-# INLINE putState #-}
putState :: S -> BitGet ()
putState s = B $ \_ -> return (s,())


-- | Get 1 bit as a 'Bool'.
{-# INLINE getBool #-}
getBool :: BitGet Bool
getBool = block bool

-- | Get a 'Float'.
{-# NOINLINE getFloat #-}
getFloat :: BitGet Float
getFloat = block float

-- | Get a 'Double'.
{-# INLINE getDouble #-}
getDouble :: BitGet Double
getDouble = block double

-- | Get a 'Word8'.
{-# INLINE getByte #-}
getByte :: BitGet Word8
getByte = block byte

-- | Get @n@ bytes as a 'ByteString'.
getByteString :: Int -> BitGet ByteString
getByteString n = block (byteString n)

-- | Get @n@ bytes as a lazy ByteString.
getLazyByteString :: Int -> BitGet L.ByteString
getLazyByteString n = do
  (S _ o) <- getState
  case o of
    0 -> B $ \ (S bs o') -> do
            putBackState bs o'
            lbs <- B.getLazyByteString (fromIntegral n)
            return (S S.empty 0, lbs)
    _ -> L.fromChunks . (:[]) <$> Data.Binary.Bits.Get.getByteString n

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: BitGet Bool
isEmpty = B $ \ (S bs o) -> if S.null bs
                               then B.isEmpty >>= \e -> return (S bs o, e)
                               else return (S bs o, False)

-- | Read a 1 bit 'Bool'.
{-# INLINE bool #-}
bool :: Block Bool
bool = Block 1 readBool

{-# INLINE byte #-}
byte :: Block Word8
byte = Block 8 readByte

{-# INLINE float #-}
float :: Block Float
float = Block 32 readFloat

{-# INLINE double #-}
double :: Block Double
double = Block 64 readDouble

-- | Read @n@ bytes as a 'ByteString'.
byteString :: Int -> Block ByteString
byteString n | n > 0 = Block (n*8) (readByteString n)
             | otherwise = Block 0 (\_ -> S.empty)

