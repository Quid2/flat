{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf ,TypeSynonymInstances ,FlexibleInstances ,BangPatterns ,NoMonomorphismRestriction #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Foldable
import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import System.IO.Unsafe
import Control.Monad
import Data.Flat.Pokes

type Encoding = Writer

bitEncoder :: Encoding -> L.ByteString
bitEncoder (Writer f) =
  let bufSize = 4096 -- 100000000
  in L.fromStrict $ BS.unsafeCreateUptoN bufSize $
     \ptr -> do
       (S ptr' 0 0) <- f (S ptr 0 0)
       return $ ptr' `minusPtr` ptr


newtype Writer = Writer (S -> IO S)
instance Show Writer where show (Writer _) = "Writer"

instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  mappend (Writer f) (Writer g) = Writer (f >=> g)

  -- {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

{-# INLINE eBool #-}
eBool False = eFalse -- eBit 1 0
eBool True = eTrue -- eBits 1 1
{-# INLINE eTrue #-}
eTrue = Writer eTrueF
{-# INLINE eFalse #-}
eFalse= Writer eFalseF
{-# INLINE eBits #-}
eBits n t = Writer (eBitsF n t)
{-# INLINE eFiller #-}
eFiller = Writer eFillerF
