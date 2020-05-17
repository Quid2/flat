{-# LANGUAGE FlexibleInstances #-}
module Data.ByteString.Convert
  ( AsByteString(..)
  )
where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import           Data.Word

-- |Convert to/from strict ByteStrings
class AsByteString a where
  toByteString :: a -> B.ByteString
  fromByteString :: B.ByteString -> a

instance AsByteString B.ByteString where
  toByteString   = id
  fromByteString = id

instance AsByteString L.ByteString where
  toByteString   = L.toStrict
  fromByteString = L.fromStrict

instance AsByteString [Word8] where
  toByteString   = B.pack
  fromByteString = B.unpack

