{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Data.Flat.Prim(Encoding,(<>),(<+>),Seq(..),(<|),mempty,bitEncoder,eBitsS,eTrueS,eListElem,eUnsigned,eUnsigned16,eUnsigned32,eUnsigned64,eWord32BE,eWord64BE,eWord8,eBits,eFiller,eBool,eTrue,eFalse,eBytes,eLazyBytes,eShortBytes,eUTF16) where

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
-- import Debug.Trace
traceShowId = id

-- encode (a,b,c) = encode a <+> encode b <+> encode b
-- encode Nil = ebits 1 -- Leaf
-- encode Cons h t = eTrue <+ h <> t
-- encode (Stream a s) = 

--{-# RULES "seq/merge" forall n1 n2 s1 s2. Node (Leaf (Step n1 s1)) (Leaf (Step n2 s2)) = Leaf (Step (n1+n2) (s1 >=> s2)) ; #-} 

-- {-# RULES "seq/merge" forall n1 n2 s1 s2. (Leaf (Step n1 s1)) <> (Leaf (Step n2 s2)) = Leaf (Step (n1+n2) (s1 >=> s2)) ; #-}

-- {-# RULES "Node t Empty" forall t. Node t Empty = t ; #-}

-- {-# RULES "Node t Empty" forall t. mappend t Empty = t ; #-}

-- reduce :: Seq Step -> Step
-- reduce (Leaf s) = s
-- reduce (Node l1 l2) = let Step n1 f1 = reduce l1
--                           Step n2 f2 = reduce l2
--                       in Step (n1+n2) (f1>=>f2)
-- reduce Empty = Step 0 return

c = bitEncoder $ eBits 2 3 <> eBits 2 0 <> eFiller
s = show $ eBits 2 3 <> eBits 2 0 <> eFiller

type Encoding = Seq Step

--{-# INLINE mempty #-}
-- mempty = Empty

infixr 6 <+>

-- Fuse together, only for non recursive staff
{-# INLINE (<+>) #-}
-- n1 <+> n2 = reduce n1 reduce n2
--(Leaf (Step n1 s1)) <+> (Leaf (Step n2 s2)) = Leaf (Step (n1+n2) (s1 >=> s2))
l1@(Leaf (Step n1 s1)) <+> l2@(Leaf (Step n2 s2)) | n1+n2<=512 = Leaf (Step (n1+n2) (s1 >=> s2))
                                                  | otherwise = Node l1 l2
n1 <+> Empty = n1
n1 <+> n2 = Node n1 n2

-- n1 <+> n2 = error $ unwords [show n1,"<>",show n2]

-- {-# INLINE (<+>) #-}
-- (<+>) = (<>)

-- {-# INLINE (<>) #-}
-- n1 <> Empty = n1
-- Leaf s <> l2 = s :<< l2
-- --l1@(Leaf (Step n1 s1)) <> l2@(Leaf (Step n2 s2)) | n1<=256 = Leaf (Step (n1+n2) (s1 >=> s2))
-- --                                                 | otherwise = Node l1 l2
-- -- Slow down lists
-- -- With this, the whole serialisation collapses into a single Step
-- n1 <> n2 = Node n1 n2

bitEncoder :: Encoding -> L.ByteString
bitEncoder = bitEncoderLazy 32 encoder

encoder :: E -> Seq Step -> IO (Signal (Seq Step))
encoder e@(E fp s) enc = go (availBits e) enc s
-- encoder e@(E fp s) enc = go (availBytes e*8) (traceShowId enc) s
--encoder e@(E fp s) enc = go (availBytes e*8) (viewList enc) s
  where
     -- NOTE: pessimistic check, wastes space
     go !nb !e !s = case viewl e of
       (Step n op :< k) | traceShowId n <= nb -> op s >>= go (nb-n) k
       --(Step n op :< k) | n <= nb -> op s >>= go (nb-n) k
                        | otherwise -> notEnoughSpace s n e
       EmptyL -> done s

     -- go !nb !e !s = case e of
     --  (Step n op : k) | n <= nb -> op s >>= go (nb-n) k
     --                  | otherwise -> notEnoughSpace s (bitsToBytes n) e
     --  [] -> done s


--leaf :: Int -> Prim -> Prim
-- leaf nb s n op k | n <= nb = op s >>= leaf (nb-n) k
--                  | otherwise = notEnoughSpace s (bitsToBytes n) e


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

eListElem (Leaf step) = Leaf (eListElemS step)
eListElem enc = Node eTrue enc

eUTF16 = Leaf . eUTF16S
eBytes = Leaf . eBytesS
eLazyBytes = Leaf . eLazyBytesS
eShortBytes = Leaf . eShortBytesS
eUnsigned = Leaf . eUnsignedS
eUnsigned64 = Leaf . eUnsigned64S
eUnsigned32 = Leaf . eUnsigned32S
eUnsigned16 = Leaf . eUnsigned16S
eWord32BE = Leaf . eWord32BES
eWord64BE = Leaf . eWord64BES
eWord8 = Leaf . eWord8S
eBits n t = Leaf (eBitsS n t)
eFiller = Leaf eFillerS
eBool = Leaf . eBoolS
eTrue = Leaf eTrueS
eFalse = Leaf eFalseS



