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
import           Data.Monoid hiding ((<>))
import           Data.Semigroup
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe

c = bitEncoder ee

ee = eBits 2 3 <> eBits 2 0 <> eFiller

eBool = undefined
eTrue = eBits 1 1
eFalse = eBits 1 0

newtype Encoding = Encoding (Tag -> Tag)

instance Semigroup Encoding where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid Encoding where
  {-# INLINE mempty #-}
  mempty = Encoding id
  {-# INLINE mappend #-}
  Encoding b1 `mappend` Encoding b2 = Encoding (b1 . b2)
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

bitEncoder :: Encoding -> L.ByteString
bitEncoder e = bitEncoder_ $ tag e

tag :: Encoding -> Tag
tag (Encoding e) = e TagEnd

data Tag = Tag8 !Int !Word8 Tag | TagFiller Tag | TagEnd

eBits :: Int -> Word8 -> Encoding
eBits n w = Encoding (Tag8 n w)

eFiller :: Encoding
eFiller = Encoding TagFiller

{-# INLINE ok #-}
ok :: Ptr Word8 -> S -> Tag -> IO (Either (Int, S, Tag) S)
ok fp s@(S op w o) ts@(Tag8 n t k) =
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
  in if | f > 0  ->  ok fp (S op (w .|. (t `shiftL` f)) o') k
        | f == 0 ->  safeNext fp s ts (w .|. (t `shiftL` f))
        | otherwise -> safePoke (t `shiftL` (8 - o')) (-f) fp s ts (w .|. (t `shiftR` o'))

ok fp s@(S op w _) ts@(TagFiller _) = safeNext fp s ts (w .|. 1)

ok _ s TagEnd = done s

{-# INLINE getK #-}
getK (Tag8 _ _ k) = k
getK (TagFiller k) = k

{-# INLINE safeNext #-}
safeNext = safePoke 0 0

{-# INLINE safePoke #-}
safePoke w' n' fp s@(S op _ _) ts w | op < fp   = poke op w >> ok fp (S (plusPtr op 1) w' n') (getK ts)
                                    | otherwise = notEnoughSpace s 1 ts



