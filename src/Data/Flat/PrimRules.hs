{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes,eee) where

import           Data.ByteString.Builder.Prim
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as L
--import qualified Data.ByteString.Lazy         as L
import           Control.Monad
import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe
import Data.Flat.Pokes

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

type Encoding = Tree

data Tree = Empty
          | Branch Tree Tree
          | Op Int (S -> IO S)
          | Op0 (S -> IO S)
          | VarOp (S -> IO (Either (Int,S,Encoding) S))
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


{-# INLINE eBits #-}
eBits !n !t = Op 1 (eBitsF n t)
{-# INLINE eFiller #-}
eFiller = Op 1 eFillerF

bitEncoder :: Encoding -> L.ByteString
bitEncoder = enc [] 4096 0 0
 where
   enc :: [BS.ByteString] -> Int -> Word8 -> Int -> Encoding -> L.ByteString
   enc bufs bufSize w u e = do
     let (bs,signal) = bufEncode bufSize w u e
     case signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       NotEnoughSpace s' n e' -> enc (bs:bufs) (max n $ bufSize*3 `div` 2) (currByte s') (usedBits s') e'

bufEncode :: Int -> Word8 -> Int -> Encoding -> (BS.ByteString,Signal)
bufEncode bufSize w u e = unsafeCreateUptoN' bufSize $
    \ptr -> do
      !s <- encoder (ptr `plusPtr` bufSize) (S ptr w u) e
      return (nextPtr (getState s) `minusPtr` ptr,s)

encoder fp = go
  where
    go s e = case viewl e of
      (n,step) :< k | nextPtr s `plusPtr` n <= fp -> do
                        when (n>1) $ print n
                        s' <- step s
                        go s' k
                    | otherwise -> notEnoughSpace s n e
      EmptyL -> done s

data Signal = Done !S | NotEnoughSpace !S !Int Encoding
done = return . Done
notEnoughSpace s n e = return (NotEnoughSpace s n e)
getState (Done s) = s
getState (NotEnoughSpace s _ _) = s

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

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
    fp <- BS.mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    return (BS.PS fp 0 l', res)
{-# INLINE createUptoN' #-}

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



