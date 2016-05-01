{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances,ScopedTypeVariables #-}
module Data.Flat.Run(flat,unflat,runGet
                    ,Decoded,DeserializeFailure
                    ,Encoded(..)
                    ,showBits
                    ,encoded,decoded
                    ) where

import           Control.Exception
import           Data.Binary.Bits.Get
import qualified Data.Binary.Get      as Get (Decoder (..), Get (..),
                                              runGetIncremental)
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           Data.Flat.Encoder
import           Data.Word
import           Text.Printf

flat :: Flat a => a -> L.ByteString
flat a = bitEncoder (encode a)

unflat :: Flat a => L.ByteString -> Decoded a
unflat = runGetOrFail decode

--unflatWith :: Get a -> L.ByteString -> a
-- unflatWith = runGet

--unflatIncremental = Get.runGetIncremental
-- runGet decode
encoded :: Flat a => a -> Encoded a
encoded = Encoded . flat

-- TODO: detect left over data and give error
decoded :: Flat a => Encoded a -> Decoded a 
decoded = unflat . bytes

-- |Encoded data, mainly useful to show data in a nicer way
newtype Encoded a = Encoded {bytes::L.ByteString}

instance Show (Encoded a) where show = unwords . map showBits . L.unpack . bytes

type Decoded a = Either DeserializeFailure a

type DeserializeFailure = String

instance Exception DeserializeFailure

-- flatten = Flat . flat

-- -- TODO: detect left over data and give error
-- unflatten = unflat . bytes

-- newtype Flat = Flat {bytes::L.ByteString}

-- instance Show Flat where
--   show = unwords . map showBits . L.unpack . bytes

showBits :: Word8 -> String
showBits = printf "%08b"
