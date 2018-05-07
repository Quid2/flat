{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf ,TypeSynonymInstances ,FlexibleInstances ,BangPatterns ,NoMonomorphismRestriction #-}
module Data.Flat.Prim(Encoding,(<>),(<+>),(<|),Step(..),(>=>),wprim,encodersR,mempty,bitEncoder,eBitsS,eTrueF,eFalseF,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16,chkWriter) where

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
import qualified Data.Flat.Pokes as P
import Control.Exception
import Data.List
-- import Debug.Trace
traceShow _ a = a

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderLazy 32 encoder

encoder :: E -> Encoding -> IO (Signal Encoding)

encoder e@(E p s) w = catch (runWriter w e >>= (\(E _ s') -> done s')) (\(NotEnoughSpaceException s neededBits ws) -> notEnoughSpace s neededBits (encoders (traceShow (unwords ["encoder",show ws]) ws)))

type Encoding = Writer

-- data W = P Step | W Writer
-- merge (P (Step n1 s1)) (P (Step n2 s2)) = P (Step $ n1+n2) (s1>=>s2)

newtype Writer = Writer {runWriter :: E -> IO E}
instance Show Writer where show (Writer _) = "Writer"

data NotEnoughSpaceException = NotEnoughSpaceException S Int [[Writer]] deriving Show
instance Exception NotEnoughSpaceException

instance Semigroup Writer where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid Writer where
  {-# INLINE mempty #-}
  mempty = Writer return

  {-# INLINE mappend #-}
  mappend (Writer f) (Writer g) = Writer (f >=> g)

  -- {-# INLINE mconcat #-}
  -- mconcat = foldl' mappend mempty

{-# INLINE chkWriter #-}
chkWriter :: Writer -> [Writer] -> Writer
chkWriter w ws = Writer $ \e -> catch (runWriter w e) (\(NotEnoughSpaceException s neededBits w') -> throw (NotEnoughSpaceException s neededBits (ws:w')))

-- chkWriter w _ = w

{-
         a      filler
    ww b1 b2 b3

b2 b3 filler

  t   a       b
    t   a   b
      t a b

  Cons !            a
       (Cons ! b)
             ()
-}

{-# RULES "encodersR5" forall a b c d e. encodersR [e,d,c,b,a] = chkWriter a [b,c,d,e] <> chkWriter b [c,d,e] <> chkWriter c [d,e] <> chkWriter d [e] <> e; #-}
{-# RULES "encodersR4" forall a b c d. encodersR [d,c,b,a] = chkWriter a [b,c,d] <> chkWriter b [c,d] <> chkWriter c [d] <> d; #-}
{-# RULES "encodersR3" forall b c d. encodersR [d,c,b] = chkWriter b [c,d] <> chkWriter c [d] <> d; #-}
{-# RULES "encodersR2" forall a b. encodersR [b,a] = chkWriter a [b] <> b; #-}
{-# RULES "encodersR1" forall a. encodersR [a] = a; #-}
{-# RULES "encodersR0" encodersR [] = mempty; #-}
-- So that RULES can work
-- DO not work in instances but they do in Class!
{-                    | Class | Instances |
NOINLINE | error      | works  | fails |
           definition | fail  | fail
Problem: encodersR rules do not fire in Instances! why?
:r
-}
{-# NOINLINE encodersR #-} -- So that RULES can work
-- {-# INLINE [0] encodersR #-}
encodersR :: [Writer] -> Writer
-- encodersR ws = encoders_ . reverse $ traceShow (unwords ["encodersR",show ws]) ws
encodersR ws = encoders_ . reverse $ ws -- traceShow (unwords ["encodersR",show ws]) ws
-- encodersR ws = error $ unwords ["encodersR CALLED",show ws]

x = encodersR [] <> encodersR [mempty]

-- {-# INLINE [0] encoders #-}
-- {-# NOINLINE encoders #-} -- So that RULES can work
encoders = encoders_ . concat . reverse
encoders_ :: [Writer] -> Writer
encoders_ [] = mempty
encoders_ [a] = a
encoders_ (x:xs) = chkWriter x xs <> encoders_ xs

{-# INLINE wprim#-}
wprim:: Step -> Writer
wprim(Step n f) = me
  where
    me = Writer prim
    prim e@(E p s) | n <= availBits e = f s >>= return . E p
                   | otherwise = throw (NotEnoughSpaceException s n [[me]])

{-# INLINE (<+>) #-}
(<+>) = (<>)
-- l <+> e = e : l

-- eBeg = encoders . []

{-# INLINE (<|) #-}
(<|) = (<>)
-- w0 <| w1 = encoders $ w1 [w0]

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
-- eListElem (Writer f) = Writer (n+1) (eTrueF >=> f) -- eListElemS e -- eTrue <> e
--eListElem wprim= eTrue <> w
eListElem w = chkWriter eTrue [w] <> w
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
