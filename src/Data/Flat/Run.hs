{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances         #-}
module Data.Flat.Run (
    flat,
    flatStrict,
    unflat,
    unflatStrict,
    unflatWith,
    unflatRaw,
    --unflatRawWith,
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           Data.Flat.Decoder
import qualified Data.Flat.Encoder    as E
import           Data.Flat.Filler

-- |Strictly encode padded value.
flatStrict :: Flat a => a -> B.ByteString
flatStrict = flat

-- |Encode padded value.
flat :: (FlatRaw (PostAligned a) c, Flat a) => a -> c
flat = flatRaw . postAligned

unflatStrict :: Flat a => B.ByteString -> Decoded a
unflatStrict = unflat

-- |Decode padded value.
unflat :: (FlatRaw (PostAligned a) b, Flat a) => b -> Decoded a
unflat = unflatWith decode

-- |Decode padded value, using the provided decoder.
unflatWith :: FlatRaw (PostAligned a) b => Get (PostAligned a) -> b -> Decoded a
unflatWith dec bs = postValue <$> unflatRawWith dec bs

-- |Decode (unpadded) value.
unflatRaw :: (FlatRaw a b, Flat a) => b -> Decoded a
unflatRaw = unflatRawWith decode

class FlatRaw a b where
  -- |Encode (unpadded) value
  flatRaw :: Flat a => a -> b

  -- |Unflat (unpadded) value, using provided decoder
  unflatRawWith :: Get a -> b -> Decoded a

instance Flat a => FlatRaw a B.ByteString where
  flatRaw a = E.strictEncoder (getSize a) (encode a)

  unflatRawWith = strictDecoder

instance Flat a => FlatRaw a L.ByteString where
  flatRaw = L.fromStrict . flatRaw
  unflatRawWith dec = unflatRawWith dec . L.toStrict
