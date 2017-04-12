{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Prim(Encoding,(<>),(<+>),(<|),Step(..),mempty,bitEncoder,eBitsS,eTrueF,eFalseF,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where
import           Control.Monad
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Foldable
--import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe
import Data.Flat.Pokes hiding (Step,eBitsS)
import qualified Data.Flat.Pokes as P 
import Prelude hiding (mempty)

c = bitEncoder $ eBits 2 3 <> eBits 2 0 <> eFiller
s = show $ eBits 2 3 <> eBits 2 0 <> eFiller

-- Naked functions
type Encoding = Step -> Step
(<>) = (.)
mempty = id

{-# INLINE (<+>) #-}
(<+>) = (<>)

infixr 5 <|

{-# INLINE (<|) #-}
(<|) = (<+>)

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

-- data StepK = StepK Int (S->IO S) StepK

-- -- checked :: Int -> (Int, S -> IO a, Int -> a -> IO (Signal e)) -> S -> IO (Signal e)
-- checked nAvail st@(StepK n op k) s | n <= nAvail = op s >>= (nAvail-n)
--                                    | otherwise = notEnoughSpace s (bitsToBytes n) st


data Step = Step !Int (S -> IO S) Step | StepEnd

instance Show (Step->Step) where show ss = show $ ss StepEnd

instance Show Step where
  show (Step n _ k) = unwords ["Step",show n,show k]
  show StepEnd = "StepEnd"

end :: Encoding -> Step
end e = e StepEnd

bitEncoder :: Encoding -> L.ByteString
bitEncoder e = bitEncoderLazy 4096 encoder (end e)

encoder e@(E fp s) = go (availBits e) s
  where
    --go s step@(Step n op k) | hasBytes e n = op s >>= go s' k
    -- NOTE: pessimistic check, wastes space
    go nb s step@(Step n op k) | n < nb = op s >>= \s' -> go (nb-n) s' k
                               | otherwise = notEnoughSpace s (bitsToBytes n) step
    go nb s StepEnd = done s

{-# INLINE eListElem #-}
{-# INLINE eUnsigned #-}
{-# INLINE eUnsigned64 #-}
{-# INLINE eUnsigned32 #-}
{-# INLINE eUnsigned16 #-}
{-# INLINE eWord32BE #-}
{-# INLINE eWord64BE #-}
{-# INLINE eWord8 #-}
{-# INLINE eFalse #-}
{-# INLINE eBits #-}
{-# INLINE eFiller #-}
{-# INLINE eBool #-}
{-# INLINE eTrue #-}
{-# INLINE eBytes #-}
{-# INLINE eLazyBytes #-}
{-# INLINE eShortBytes #-}
{-# INLINE eUTF16 #-}
st (P.Step n op) = Step n op

eBitsS = eBits
--eListElem (Step n f k) = Step (n+1) (eTrueF >=> f)
eListElem st = eTrue <> st
eUTF16 = st . eUTF16S
eBytes = st . eBytesS
eLazyBytes = st . eLazyBytesS
eShortBytes = st . eShortBytesS
eUnsigned = st . eUnsignedS
eUnsigned64 = st . eUnsigned64S
eUnsigned32 = st . eUnsigned32S
eUnsigned16 = st . eUnsigned16S
eWord32BE = st . eWord32BES
eWord64BE = st . eWord64BES
eWord8 = st . eWord8S
eBits n t = st (P.eBitsS n t)
eFiller = st eFillerS
eBool = st . eBoolS
eTrue = st eTrueS
eFalse = st eFalseS

