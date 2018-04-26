{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf ,TypeSynonymInstances ,FlexibleInstances ,BangPatterns ,NoMonomorphismRestriction #-}
module Data.Flat.Prim(Encoding,(<>),(<+>),(<|),Step(..),wprim,(>=>),chkWriter,encodersR,mempty,bitEncoder,eBitsS,eTrueF,eFalseF,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Foldable
import           Data.Monoid hiding ((<>))
import           Data.Semigroup
import           Data.Word
import           Foreign
import           Foreign.Ptr
import System.IO.Unsafe
import Control.Monad
import Data.Flat.Pokes hiding (eBitsS)
import Data.Flat.Pokes hiding (eBitsS)
import qualified Data.Flat.Pokes as P
import Control.Exception
-- import Data.Flat.Class
-- import Debug.Trace
traceShow _ a = a

type Encoding = Writer


{-# RULES
"encodersR12" forall a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12. encodersR [a12,a11,a10,a9,a8,a7,a6,a5,a4,a3,a2,a1] = a1 <> a2 <> a3 <> a4 <> a5 <> a6 <> a7 <> a8 <> a9 <> a10 <> a11 <> a12
"encodersR6" forall a1 a2 a3 a4 a5 a6. encodersR [a6,a5,a4,a3,a2,a1] = a1 <> a2 <> a3 <> a4 <> a5 <> a6
"encodersR5" forall a b c d e. encodersR [e,d,c,b,a] = a <> b <> c <> d <> e
"encodersR4" forall a b c d. encodersR [d,c,b,a] = a <> b <> c <> d
"encodersR3" forall b c d. encodersR [d,c,b] = b <> c <> d
"encodersR2" forall c d. encodersR [d,c] = c <> d
"encodersR1" forall d. encodersR [d] = d
"encodersR0" encodersR [] = mempty
#-}


{-# INLINE chkWriter #-}
chkWriter w _ = w

{-# INLINE [0] encodersR #-}
encodersR :: [Writer] -> Writer
encodersR ws = mconcat . reverse $ ws -- without the explicit parameter the rules won't fire
-- encodersR ws = error $ unwords ["encodersR CALLED",show ws]

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderRetry encoder

encoder :: E -> Encoding -> IO (Signal Encoding)
-- encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(RetryException neededBits) -> return (Retry neededBits))
encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(RetryException neededBits s) -> notEnoughSpace s neededBits w)
-- encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (w))

-- bitEncoder :: Encoding -> L.ByteString
-- bitEncoder = bitEncoderLazy (3000000) encoder

-- encoder :: E -> Encoding -> IO (Signal Encoding)
-- encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (w))

newtype Writer = Writer {runWriter::E -> IO E}
instance Show Writer where show (Writer _) = "Writer"

instance Semigroup Writer where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  mappend (Writer f) (Writer g) = Writer (f >=> g)

  {-# INLINE mconcat #-}
  mconcat = foldl' mappend mempty

data RetryException = RetryException Int S deriving Show
instance Exception RetryException

{-# INLINE wprim#-}
wprim:: Step -> Writer
wprim(Step n f) = Writer prim
  where
    prim e@(E p s) | n <= availBits e = f s >>= return . E p
                   | otherwise = throw (RetryException n s)


{-# INLINE w #-}
w = Writer

{-# INLINE (<+>) #-}
(<+>) = (<>)

{-# INLINE (<|) #-}
(<|) = (<+>)

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
{-# INLINE eListElem #-}
{-# INLINE eBitsS #-}


eBitsS = eBits
eListElem e = eTrue <> e
eUTF16 = wprim . eUTF16S
eBytes = wprim . eBytesS
eLazyBytes = wprim. eLazyBytesS
eShortBytes = wprim. eShortBytesS
eUnsigned = wprim. eUnsignedS
eUnsigned64 = wprim. eUnsigned64S
eUnsigned32 = wprim. eUnsigned32S
eUnsigned16 = wprim. eUnsigned16S
eWord32BE = wprim. eWord32BES
eWord64BE = wprim. eWord64BES
eWord8 = wprim. eWord8S
eBits n t = wprim(P.eBitsS n t)
eFiller = wprim eFillerS
eBool b = wprim(eBoolS b)
eTrue = wprim eTrueS
eFalse = wprim eFalseS
