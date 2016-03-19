{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Flat.Run(flat,unflat
                    ,Decoded
                    ,showBits) where

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

--unflatIncremental = Get.runGetIncremental
-- runGet decode

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
