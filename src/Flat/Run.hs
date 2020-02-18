{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- |Encoding and decoding functions
module Flat.Run (
    flat,
    flatRaw,
    unflat,
    unflatWith,
    unflatRaw,
    unflatRawWith,
    ) where

import qualified Data.ByteString         as B
import           Data.ByteString.Convert
import           Flat.Class
import           Flat.Decoder
import qualified Flat.Encoder       as E
import           Flat.Filler

-- |Encode padded value.
flat :: Flat a => a -> B.ByteString
flat = flatRaw . postAligned

-- |Decode padded value.
unflat :: (Flat a,AsByteString b) => b -> Decoded a
unflat = unflatWith decode

-- |Decode padded value, using the provided unpadded decoder.
unflatWith :: AsByteString b => Get a -> b -> Decoded a
unflatWith dec = unflatRawWith (postAlignedDecoder dec)

-- |Decode unpadded value.
unflatRaw :: (Flat a,AsByteString b) => b -> Decoded a
unflatRaw = unflatRawWith decode

-- |Unflat unpadded value, using provided decoder
unflatRawWith :: AsByteString b => Get a -> b -> Decoded a
unflatRawWith dec = strictDecoder dec . toByteString

-- |Encode unpadded value
flatRaw :: (Flat a, AsByteString b) => a -> b
flatRaw a = fromByteString $ 
    E.strictEncoder 
        (getSize a) 
#ifdef ETA_VERSION    
        (E.trampolineEncoding (encode a))
#else
        (encode a)
#endif

-- #ifdef ETA_VERSION    
--   deriving (Show, Eq, Ord, Typeable, Generic, NFData)

-- instance Flat a => Flat (PostAligned a) where
--   encode (PostAligned val fill) = trampolineEncoding (encode val) <> encode fill

-- #else
--   deriving (Show, Eq, Ord, Typeable, Generic, NFData,Flat)
-- #endif



