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

-- List representation

c = bitEncoder ee

ee = eBits 2 3 <> eBits 2 0 <> eFiller

eBool = undefined
eTrue = eBits 1 1
eFalse = eBits 1 0
eWord8= undefined
eWord32=undefined
eWord64=undefined
eUnsigned=undefined
eLazyBytes = undefined
eBytes = undefined

data S = S {nextPtr  :: !(Ptr Word8)
           ,currByte:: !Word8
           ,usedBits:: !Int}

type Tags = [Tag]

type Encoding = Tags -> Tags

encoding ts = ts []

data Tag = Tag8 !Int !Word8 | TagFiller

eBits n w = (Tag8 n w :)

eFiller = (TagFiller :)

ok :: Ptr Word8 -> S -> [Tag] -> IO (Either (Int, S, [Tag]) S)
ok fp s@(S op w o) ts@(Tag8 n t:k) =
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
  in if | f > 0  ->  ok fp (S op (w .|. (t `shiftL` f)) o') k
        | f == 0 ->  safeNext fp s ts (w .|. (t `shiftL` f))
        | otherwise -> safePoke (t `shiftL` (8 - o')) (-f) fp s ts (w .|. (t `shiftR` o'))

ok fp s@(S op w _) ts@(TagFiller:_) = safeNext fp s ts (w .|. 1)

ok _ s [] = return . Right $ s

request = return . Left

{-# INLINE safeNext #-}
safeNext = safePoke 0 0

{-# INLINE safePoke #-}
safePoke w' n' fp s@(S op _ _) ts@(tag:k) w | op < fp   = poke op w >> ok fp (S (plusPtr op 1) w' n') k
                                            | otherwise = request (1,s,ts)


--bitEncoder_ :: [Tags] -> L.ByteString
bitEncoder = bitEncoder_ . encoding

bitEncoder_ = enc [] 10000 0 0
 where
   enc :: [BS.ByteString] -> Int -> Word8 -> Int -> Tags -> L.ByteString
   enc bufs bufSize w u e = do
     let (bs,(s',mc)) = bufEncode bufSize w u e
     case mc of
       Nothing | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       Just (n,e') -> enc (bs:bufs) (max n $ bufSize*3 `div` 2) (currByte s') (usedBits s') e'

bufEncode :: Int -> Word8 -> Int -> Tags -> (BS.ByteString,(S, Maybe (Int,Tags)))
bufEncode bufSize w u e = unsafeCreateUptoN' bufSize $
    \ptr -> do
      (!s,!mc) <- encStep (ptr `plusPtr` bufSize) (S ptr w u) e
      return (nextPtr s `minusPtr` ptr,(s,mc))

encStep fp s e = ok fp s e >>= either (\(n,s,cont) -> return (s,Just (n,cont))) (\s -> return (s,Nothing))


unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
    fp <- BS.mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    return (BS.PS fp 0 l', res)
{-# INLINE createUptoN' #-}


