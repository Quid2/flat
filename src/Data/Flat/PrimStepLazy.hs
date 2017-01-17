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
import Data.Flat.Pokes
import Prelude hiding (mempty)
import Data.Seq

c = bitEncoder $ eBits 2 3 <> eBits 2 0 <> eFiller
s = show $ eBits 2 3 <> eBits 2 0 <> eFiller

-- Naked functions
type Encoding = Seq Step

data Step = Step !Int (S -> IO S)

instance Show Step where show (Step n _) = unwords ["Step",show n]

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderLazy 1 encoder

encoder e@(E fp s) = go (availBytes e*8) s
  where
    -- NOTE: pessimistic check, wastes space
    go !nb !s !e = case viewl e of
      (Step n op :< k) | n < nb -> op s >>= \s' -> go (nb-n) s' k
                       | otherwise -> notEnoughSpace s (bitsToBytes n) e
      EmptyL -> done s

{-# INLINE eBits #-}
eBits n t = Leaf $ Step n $ eBitsF n t

{-# INLINE eFiller #-}
eFiller = Leaf $ Step 8 eFillerF

{-# INLINE eBool #-}
eBool = Leaf . Step 1 . eBoolF

{-# INLINE eTrue #-}
eTrue = Leaf $ Step 1 eTrueF

{-# INLINE eFalse #-}
eFalse = Leaf $ Step 1 eFalseF

--eLazyBytes bs = Step (L.length bs+1) ()

