{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Flat.Memory(chunksToByteString,chunksToByteArray,ByteArray(..)) where

import           Control.Monad
import qualified Data.ByteString.Internal as BS
import           Data.Primitive.ByteArray
import           Foreign                  hiding (void)
import           Control.Monad.Primitive (PrimMonad (..))
import           GHC.Prim (copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.Types (IO(..), Int(..))
import System.IO.Unsafe

-- toByteString :: Ptr Word8 -> Int -> BS.ByteString
-- toByteString sourcePtr sourceLength = BS.unsafeCreate sourceLength $ \destPointer -> BS.memcpy destPointer sourcePtr sourceLength

chunksToByteString :: (Ptr Word8,[Int]) -> BS.ByteString
chunksToByteString (sourcePtr,lens) =
  BS.unsafeCreate (sum lens) $ \destPtr -> void $ foldM (\(destPtr,sourcePtr) sourceLength -> BS.memcpy destPtr sourcePtr sourceLength >> return (destPtr `plusPtr` sourceLength,sourcePtr `plusPtr` (sourceLength+1))) (destPtr,sourcePtr) lens

chunksToByteArray :: (Ptr Word8,[Int]) -> (ByteArray,Int)
chunksToByteArray (sourcePtr,lens) = unsafePerformIO $ do
  let len = sum lens
  arr <- newByteArray len
  foldM_ (\(destOff,sourcePtr) sourceLength -> copyAddrToByteArray sourcePtr arr destOff sourceLength >> return (destOff + sourceLength,sourcePtr `plusPtr` (sourceLength+1))) (0,sourcePtr) lens
  farr <- unsafeFreezeByteArray arr
  return (farr,len)

-- from store-core
-- | Wrapper around @copyAddrToByteArray#@ primop.
copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
{-# INLINE copyAddrToByteArray  #-}
