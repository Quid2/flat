{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf ,TypeSynonymInstances ,FlexibleInstances ,BangPatterns ,NoMonomorphismRestriction #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes,ensureByte) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Foldable
--import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import System.IO.Unsafe
-- import Control.Monad.Cont
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors 
import Control.Monad.Trans.Class
import Control.Monad
import Prelude hiding (mempty)

--bitEncoderL :: WriterK -> L.ByteString
-- bitEncoderL c = enc [] f 0 0
--   where
--     bufSize = 100000000
--     enc :: [BS.ByteString] -> WriterK -> Word8 -> Int -> L.ByteString
--     enc bufs (WriterK k) w n =
--       let (bs,mr) = unsafeCreateUptoN' bufSize $ \ptr -> c (S (ptr `plusPtr` bufSize) ptr w n)
--             case er of
--               Right (S fp ptr' 0 0)  -> return (ptr' `minusPtr` ptr,Nothing)
--               Left (k,S fp ptr' w n) -> return (ptr' `minusPtr` ptr,Just (k,w,n))
--       in case mr of
--         Nothing -> L.fromChunks (reverse $ bs:bufs)
--         Just (k,w,n) -> enc (bs:bufs) k w n

-- mempty :: Encoding
mempty = return

(<>) = (>=>)

c = bitEncoder encodeC

encodeC  :: Encoding
encodeC s = ensureByte s >>= eFalse >>= eTrue >>= eTrue >>= eFiller

--encoder :: EncodingC -> IO S
-- encoder :: Encoding -> S -> IO S
-- encoder e s = pogoStick (\(Request (n,s) cont) -> lift (print n) >> cont s) (e s)

type Encoding = S -> Coroutine (Request (Int,S) S) IO S

-- newtype Writer = Writer (S -> Coroutine (Request (Int,S) S) IO S)
-- instance Show Writer where show (Writer _) = "Writer"

-- instance Monoid Writer where
--   {-# INLINE mempty #-}
--   mempty = Writer return

--   {-# INLINE mappend #-}
--   mappend (Writer f) (Writer g) = Writer (f >=> g)

bitEncoder :: Encoding -> L.ByteString
bitEncoder = enc [] 4096 0 0
 where
   enc :: [BS.ByteString] -> Int -> Word8 -> Int -> Encoding -> L.ByteString
   enc bufs bufSize w u e = do
     let (bs,(s',mc)) = bufEncode bufSize w u e
     case mc of
       Nothing | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       Just (n,e') -> enc (bs:bufs) (max n $ bufSize*3 `div` 2) (currByte s') (usedBits s') e'

bufEncode :: Int -> Word8 -> Int -> Encoding -> (BS.ByteString,(S, Maybe (Int,Encoding)))
bufEncode bufSize w u e = unsafeCreateUptoN' bufSize $
    \ptr -> do
      (!s,!mc) <- encStep e (S (ptr `plusPtr` bufSize) ptr w u)
      return (nextPtr s `minusPtr` ptr,(s,mc))

encStep e s = resume (e s) >>= either (\(Request (n,s) cont) -> return (s,Just (n,cont))) (\s -> return (s,Nothing))

{-# INLINE ensureByte #-}
ensureByte :: Encoding
ensureByte s | nextPtr s < lastPtr s = return s
             | otherwise = request (1,s)

ensureSize :: Int -> Encoding
ensureSize n s | nextPtr s `plusPtr` n < lastPtr s = return s
               | otherwise = request (n,s)

data S = S {lastPtr :: !(Ptr Word8)
           ,nextPtr :: !(Ptr Word8)
           ,currByte:: !Word8
           ,usedBits:: !Int}


unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
    fp <- BS.mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    return (BS.PS fp 0 l', res)
{-# INLINE createUptoN' #-}


--Bytes :: Int -> S -> (Either )IO S
hasBytes n s = nextPtr s `plusPtr` n <= lastPtr s

eBool = undefined
--eFalse = undefined
--eTrue = undefined
eWord8= undefined
eWord32=undefined
eWord64=undefined
eUnsigned=undefined
eLazyBytes = undefined
eBytes = undefined

--{-# INLINE eBits #-}
eTrue :: Encoding
eTrue = ensureByte >=> lift . eTrueF

eFalse :: Encoding
eFalse = ensureByte >=> lift . eFalseF

eBits :: Int -> Word8 -> Encoding
eBits n t = ensureByte >=> lift . eBitsF n t

eFiller :: Encoding
eFiller = lift . eFillerF

{-# INLINE eBitsF #-}
eBitsF n t = \(S fp op w o) ->
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
  in if | f > 0  ->  return $ S fp op (w .|. (t `shiftL` f)) o'
        | f == 0 ->  pokeWord fp op (w .|. (t `shiftL` f))
        | otherwise -> poke op (w .|. (t `shiftR` o')) >> return (S fp (plusPtr op 1) (t `shiftL` (8 - o')) (-f))

{-# INLINE eTrue #-}
eTrueF s@(S fp op w o) =
  let w' = (currByte s `shiftL` 1) .|. 1
  in if usedBits s == 7
     then pokeWord fp op w'
     else return (S fp op w' (o+1)) --s {currByte=w',usedBits=usedBits s +1}

{-# INLINE eFalse #-}
eFalseF (S fp op w o) | o == 7 = pokeWord fp op w
                      | otherwise = return (S fp op w (o+1))

eFillerF (S fp op w _) = pokeWord fp op (w .|. 1)

{-# INLINE pokeWord #-}
pokeWord fp op w = poke op w >> return (S fp (plusPtr op 1) 0 0)


