{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes) where

import           Control.Monad
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Foldable
import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe

-- Same as PrimTag, using functions rather than constructors (slower on list and same speed on tree?)
c = bitEncoder $ eBits 2 3 <> eBits 2 0 <> eFiller

eBool = undefined
eTrue = eBits 1 1
eFalse = eBits 1 0
eWord8= undefined
eWord32=undefined
eWord64=undefined
eUnsigned=undefined
eLazyBytes = undefined
eBytes = undefined
--eFiller = undefined;eBits = undefined

data S = S {nextPtr  :: !(Ptr Word8)
           ,currByte:: !Word8
           ,usedBits:: !Int}

newtype Encoding = Encoding (Step -> Step)

instance Monoid Encoding where
  {-# INLINE mempty #-}
  mempty = Encoding id -- (\k -> (\s -> k s))
  {-# INLINE mappend #-}
  Encoding f `mappend` Encoding g = Encoding (f . g)
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

newtype Tag = Tag {step :: Step}

type Step = Ptr Word8 -> S -> IO (Either (Int, S, Tag) S)

tag :: Encoding -> Tag
tag (Encoding e) = Tag $ e (\fp s -> return $ Right s)

request = return . Left

--{-# INLINE eBits #-}
eBits :: Int -> Word8 -> Encoding
eBits n w = Encoding (bitsF n w)

--{-# INLINE eFiller #-}
eFiller :: Encoding
eFiller = Encoding fillerF

--{-# INLINE bitsF #-}
bitsF n t k fp s@(S op w o) =
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
      me = bitsF n t
  in if | f > 0  ->  k fp (S op (w .|. (t `shiftL` f)) o')
        | f == 0 ->  safeNext fp s me k (w .|. (t `shiftL` f))
        | otherwise -> safePoke (t `shiftL` (8 - o')) (-f) fp s me k (w .|. (t `shiftR` o'))

--{-# INLINE fillerF #-}
fillerF k fp s@(S op w _) = safeNext fp s fillerF k (w .|. 1)

{-# INLINE safeNext #-}
safeNext = safePoke 0 0

{-# INLINE safePoke #-}
safePoke w' n' fp s@(S op _ _) me k w | op < fp   = poke op w >> k fp (S (plusPtr op 1) w' n')
                                      | otherwise = request (1,s,Tag $ me k)

bitEncoder :: Encoding -> L.ByteString
bitEncoder e = bitEncoder_ $ tag e

bitEncoder_ :: Tag -> L.ByteString
bitEncoder_ = enc [] 10000 0 0
 where
   enc :: [BS.ByteString] -> Int -> Word8 -> Int -> Tag -> L.ByteString
   enc bufs bufSize w u e = do
     let (bs,(s',mc)) = bufEncode bufSize w u e
     case mc of
       Nothing | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       Just (n,e') -> enc (bs:bufs) (max n $ bufSize*3 `div` 2) (currByte s') (usedBits s') e'

bufEncode :: Int -> Word8 -> Int -> Tag -> (BS.ByteString,(S, Maybe (Int,Tag)))
bufEncode bufSize w u e = unsafeCreateUptoN' bufSize $
    \ptr -> do
      (!s,!mc) <- encStep (ptr `plusPtr` bufSize) (S ptr w u) e
      return (nextPtr s `minusPtr` ptr,(s,mc))

encStep fp s e = step e fp s >>= either (\(n,s,cont) -> return (s,Just (n,cont))) (\s -> return (s,Nothing))

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
    fp <- BS.mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    return (BS.PS fp 0 l', res)
{-# INLINE createUptoN' #-}


