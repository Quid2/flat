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
--import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe
import Data.Flat.Pokes
import Prelude hiding (mempty)

c = bitEncoder $ eBits 2 3 <> eBits 2 0 <> eFiller
s = show $ eBits 2 3 <> eBits 2 0 <> eFiller

-- Naked functions
type Encoding = Step -> Step
(<>) = (.)
mempty = id

-- Wrapped Function
--newtype Encoding = Encoding (Step -> Step)
--instance Show Encoding where show (E s) = show (end e) --"Encoding"
-- instance Monoid Encoding where
--   {-# INLINE mempty #-}
--   mempty = Encoding id -- (\k -> (\s -> k s))
--   {-# INLINE mappend #-}
--   Encoding f `mappend` Encoding g = Encoding (f . g)
--   {-# INLINE mconcat #-}
--   mconcat = foldr mappend mempty
--end (Encoding e) = e StepEnd

data Step = Step !Int (S -> IO S) Step | StepEnd

instance Show (Step->Step) where show ss = show $ ss StepEnd

instance Show Step where
  show (Step n _ k) = unwords ["Step",show n,show k]
  show StepEnd = "StepEnd"

end :: Encoding -> Step
end e = e StepEnd

bitEncoder :: Encoding -> L.ByteString
bitEncoder e = bitEncoderLazy 4096 encoder (end e)

encoder e@(E fp s) = go (availBytes e*8) s
  where
    --go s step@(Step n op k) | hasBytes e n = op s >>= go s' k
    -- NOTE: pessimistic check, wastes space
    go nb s step@(Step n op k) | n < nb = op s >>= \s' -> go (nb-n) s' k
                               | otherwise = notEnoughSpace s (bitsToBytes n) step
    go nb s StepEnd = done s

{-# INLINE eBits #-}
eBits n t = Step n $ eBitsF n t

{-# INLINE eFiller #-}
eFiller = Step 8 eFillerF

{-# INLINE eBool #-}
eBool = Step 1 . eBoolF

{-# INLINE eTrue #-}
eTrue = Step 1 eTrueF

{-# INLINE eFalse #-}
eFalse = Step 1 eFalseF

--eLazyBytes bs = Step (L.length bs+1) ()

