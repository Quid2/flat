{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes,eee) where

import           Data.ByteString.Builder.Prim
import qualified Data.ByteString.Internal     as BS

--import qualified Data.ByteString.Lazy         as L

import           Data.Monoid

import           Data.Word
import           Foreign
import           Foreign.Ptr

import Data.Proxy
import qualified Data.Seq as S

{-# INLINE eBool #-}
eBool False = eFalse
eBool True  = eTrue
{-# INLINE eTrue #-}
eTrue = Op 1 eTrueF
{-# INLINE eFalse #-}
eFalse= Op 1 eFalseF
eWord8= undefined
eWord32=undefined
eWord64=undefined
eUnsigned=undefined
eLazyBytes = undefined
eBytes = undefined

eBoolF False = eFalseF
eBoolF True  = eTrueF

-- type CEnc a = S.Seq (Prim a)
type Sized a = Prim a

data Prim a = CPrim !Int (a -> S -> IO S)
            | VPrim (a -> Step)

newtype Step = Step {step :: Ptr Word8 -> S -> IO (SignalA Step)}

type SignalC = SignalA Step



runPrim :: Prim a -> Step
runPrim c@(CPrim _ _) = checkedPrim c
runPrim (VPrim p) = step p

checkedPrim (CPrim n st) fp s | nextPtr s `plusPtr` n <= fp = Done <$> st s
                              | otherwise = notEnoughSpace s n step

--comb :: Step -> Step -> Step
combVPrims s1 s2 = (\fp s -> do
                       sig <- s1 fp s
                       case sig of
                         Done s1 -> s2 fp s1
                         NotEnoughSpace s' n k -> NotEnoughSpace s' n (combVPrims k s2))


combine :: forall a b c. (c->a) -> (c->b) -> Prim a -> Prim b -> Prim c
combine toA toB (CPrim n1 s1) (CPrim n2 s2) = CPrim (n1+n2) (\c -> s1 (toA c) >=> s2 (toB c))
-- combine toA toB cp@(CPrim _ _) (VPrim s2) = VPrim (\c -> combVPrims (checkedPrim cp $ toA c) (s2 (toB c))


class HasSize a where
  sized :: Sized a

instance HasSize Bool where sized = CPrim 1 eBoolF

data TT = T1 Bool | T2 Bool Bool

instance HasSize TT where
  sized = CPrim 3 (\t -> case t of
                           T1 b ->  eFalseF >=> eBoolF b
                           T2 b1 b2 -> eTrueF >=> eBoolF b1 >=> eBoolF b2)


data L a = N | C a (L a)

-- instance HasSize a => HasSize (L a) where
--   sized = VPrim (\t -> case t of
--                          N -> eFalseF
--                          C h t -> ETrueF >enc h 


-- instance HasSize a => HasSize (L a)  where
--   Sized (undefined) (\t -> case t of
--                              N -> eFalse
--                              C v l -> sized v )


-- instance HasSize a => HasSize [a] where
--   sized =
--     let (Sized sz p) = sized (undefined :: a)
--     in Sized (\l -> appSize (length l *) sz) undefined -- (sequence repeat p

-- data Sized a = Sized (Size a) (Prim a)

data Size a
    = VarSize (a -> Int)
    | ConstSize !Int

appSize f (ConstSize n) = ConstSize (f n)
appSize f (VarSize s) = VarSize (f . s)




type Encoding = Tree

data Tree = Empty
          | Branch Tree Tree
          | Op Int (S -> IO S) -- Op that produces at most n bits (bytes?)
          | Op0 (S -> IO S)
          | VarOp (S -> IO Signal) -- Op that does its own size check
          | EnsureBytes !Int
          | Tag8 !Int !Word8
          | TagFiller
          -- | TagBytes !B.ByteString
          --deriving Show

data ViewL
  = EmptyL        -- ^ empty sequence
  | (Int,S->IO S) :< Tree    -- ^ leftmost element and the rest of the sequence

viewl (Branch (Branch l r) z) = viewl (Branch l (Branch r z))
viewl (Branch (Op n f) r) = (n, f) :< r
viewl (Op n f)            = (n,f) :< Empty
viewl Empty               = EmptyL
viewl (Branch Empty r) = viewl r

-- next f (Branch (Branch l r) z) = next f (Branch l (Branch r z))
-- next f (Branch op@(Op _ _) r)  = f op  `j` next r
-- next f (Branch Empty r)      = next f r
-- next _ Empty               = return ()
-- next f op@(Op _ _) = f op


instance Show Tree where
   show Empty = "Empty"
   show (Branch f g) = unwords ["Branch (",show f,") (",show g,")"]
   show (Op0 _) = "Op0"
   show (Op n _) = unwords ["Op",show n]
   show (VarOp _) = "VarOp"
   show (EnsureBytes n) = unwords ["EnsureBytes",show n]
   show (Tag8 n w) = unwords ["Tag8",show n,show w]
   show (TagFiller) = "TagFiller"

{-# RULES "Branch t Empty" forall t. Branch t Empty = t ; #-}
{-# RULES "tree/chks" forall n1 n2 s1 s2. Branch (Op n1 s1) (Op n2 s2) = Op (n1+n2) (s1 >=> s2) ; #-}
--{-# RULES "tree/chks0" forall s1 s2. Branch (Op0 s1) (Op0 s2) = Op0 (s1 >=> s2) ; #-}


eee = Branch (eBits 2 1) eFiller

instance Monoid Tree where
  {-# INLINE mempty #-}
  mempty = Empty

  {-# INLINE mappend #-}
  mappend = Branch




encoder fp = go
  where
    go s e = case viewl e of
      (n,step) :< k | nextPtr s `plusPtr` n <= fp -> do
                        when (n>1) $ print n
                        s' <- step s
                        go s' k
                    | otherwise -> notEnoughSpace s n e
      EmptyL -> done s

type Signal = SignalA Encoding


-- encoder fp = go
--    where
--      go s e = case e of
--        (Branch (Branch l r) z) -> go (Branch l (Branch r z))
--        (Branch Empty r) -> go r
--        k@(Branch op r) -> opr op (Just k)
--        Empty -> return (s,Nothing)
--        op -> opr op Nothing
--        -- unhandled: (Op _ _)
--        -- op -> opr op Nothing
--        opr (Op n step) mk | nextPtr s < fp = do
--                               when (n>1) $ print n
--                               return (step s,Nothing)
--                           | otherwise = return (s,(n,) <$> mk)
--        cont (Just k) s' = go 


-- bitEncoder :: Encoding -> L.ByteString
-- bitEncoder = L.fromStrict . bitEncoderStrict

-- bitEncoderStrict :: Encoding -> BS.ByteString
-- bitEncoderStrict e =
--   --let bufSize = n `div` 8 + 1
--   let bufSize = 4096 -- 10000000
--   in BS.unsafeCreateUptoN bufSize $
--      \ptr -> do
--        --(S ptr' 0 0) <- sfoldlM eBitsF eFillerF ensureBytesF (S ptr 0 0) e
--        (S ptr' 0 0) <- encoder e (S ptr 0 0)
--        return $ ptr' `minusPtr` ptr

-- encoder e fp s =
--   case e of
--     Branch tree1 tree2 -> encoder tree1 s >>= encoder tree2
--     --Op n step          -> print n >> step s
--     Op n step | nextPtr s < fp        -> do
--       when (n>1) $ print n
--       step s
--     Empty -> return s



