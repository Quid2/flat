{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnboxedTuples #-}
{- |
Memory access primitives.

Includes code from the store-core package.
-}
module Flat.Memory
  ( chunksToByteString
  , chunksToByteArray
  , ByteArray
  , pokeByteArray
  , pokeByteString
  , unsafeCreateUptoN'
  , minusPtr
  --, peekByteString
  )
where

import Control.Monad ( foldM, foldM_, void, when )
import           Control.Monad.Primitive        ( PrimMonad(..) )
import qualified Data.ByteString.Internal      as BS
import           Data.Primitive.ByteArray       ( MutableByteArray(..)
                                                , ByteArray#
                                                , ByteArray
                                                , newByteArray
                                                , unsafeFreezeByteArray
                                                )
import Foreign ( Word8, Ptr, withForeignPtr, minusPtr, plusPtr )
import           GHC.Prim                       ( copyAddrToByteArray#
                                                , copyByteArrayToAddr#
                                                )
import           GHC.Ptr                        ( Ptr(..) )
import           GHC.Types                      ( IO(..)
                                                , Int(..)
                                                )
import System.IO.Unsafe ( unsafeDupablePerformIO, unsafePerformIO )
import qualified Data.ByteString               as B

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (BS.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (BS.ByteString, a)
createUptoN' l f = do
  fp        <- BS.mallocByteString l
  (l', res) <- withForeignPtr fp $ \p -> f p
  --print (unwords ["Buffer allocated:",show l,"bytes, used:",show l',"bytes"])
  when (l' > l) $ error
    (unwords
      ["Buffer overflow, allocated:", show l, "bytes, used:", show l', "bytes"]
    )
  return (BS.PS fp 0 l', res) -- , minusPtr l')
{-# INLINE createUptoN' #-}

-- |Copy bytestring to given pointer, returns new pointer
pokeByteString :: B.ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeByteString (BS.PS foreignPointer sourceOffset sourceLength) destPointer =
  do
    withForeignPtr foreignPointer $ \sourcePointer -> BS.memcpy
      destPointer
      (sourcePointer `plusPtr` sourceOffset)
      sourceLength
    return (destPointer `plusPtr` sourceLength)

-- | Create a new bytestring, copying from sourcePtr sourceLength number of bytes
-- peekByteString :: Ptr Word8 -> Int -> BS.ByteString
-- peekByteString sourcePtr sourceLength = BS.unsafeCreate sourceLength $ \destPointer -> BS.memcpy destPointer sourcePtr sourceLength

pokeByteArray :: ByteArray# -> Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
pokeByteArray sourceArr sourceOffset len dest = do
  copyByteArrayToAddr sourceArr sourceOffset dest len
  let !dest' = dest `plusPtr` len
  return dest'
{-# INLINE pokeByteArray #-}

-- | Wrapper around @copyByteArrayToAddr#@ primop.
-- Copied from the store-core package
copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
  IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))
{-# INLINE copyByteArrayToAddr #-}

chunksToByteString :: (Ptr Word8, [Int]) -> BS.ByteString
chunksToByteString (sourcePtr0, lens) =
  BS.unsafeCreate (sum lens) $ \destPtr0 -> foldM_
    (\(destPtr, sourcePtr) sourceLength ->
      BS.memcpy destPtr sourcePtr sourceLength
        >> return
             ( destPtr `plusPtr` sourceLength
             , sourcePtr `plusPtr` (sourceLength + 1)
             )
    )
    (destPtr0, sourcePtr0)
    lens

chunksToByteArray :: (Ptr Word8, [Int]) -> (ByteArray, Int)
chunksToByteArray (sourcePtr0, lens) = unsafePerformIO $ do
  let len = sum lens
  arr <- newByteArray len
  foldM_
    (\(destOff, sourcePtr) sourceLength ->
      copyAddrToByteArray sourcePtr arr destOff sourceLength >> return
        (destOff + sourceLength, sourcePtr `plusPtr` (sourceLength + 1))
    )
    (0, sourcePtr0)
    lens
  farr <- unsafeFreezeByteArray arr
  return (farr, len)


-- | Wrapper around @copyAddrToByteArray#@ primop.
-- Copied from the store-core package
copyAddrToByteArray
  :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
  IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
{-# INLINE copyAddrToByteArray #-}
