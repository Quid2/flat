{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Run(flat,unflat,flatRaw,unflatRaw
                    ,DeserializeFailure
                    ,runGet
                    --,Encoded(..),encoded,decoded
                    ,Decoded
                    ,prettyWord8
                    ) where

import           Control.Exception
import           Data.Binary.Bits.Get
import qualified Data.Binary.Get                as Get (Decoder (..), Get (..),
                                                        runGetIncremental)
import qualified Data.ByteString.Lazy           as L
import           Data.Flat.Class
import           Data.Flat.Encoder
import           Data.Flat.Filler
import           Data.Word
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

-- |Encode byte-padded value.
flat :: Flat a => a -> L.ByteString
flat = flatRaw . postAligned

-- |Decode byte-padded value
unflat :: Flat a => L.ByteString -> Decoded a
unflat bs = (\(PostAligned v _) -> v) <$> unflatChk bs

-- Encode value, if values does not end on byte boundary, 0s padding is added.
flatRaw :: Flat a => a -> L.ByteString
flatRaw = bitEncoder . encode

unflatRaw :: Flat a => L.ByteString -> Decoded a
unflatRaw = runGetOrFail decode

--unflatRawWith :: Get a -> L.ByteString -> a
--unflatRawWith = runGet

-- unflat = runGetOrFail decode
-- unflat bs = case unflatRaw bs of
--               Left e -> Left e
--               Right (PostAligned v _,bs,n) | L.null bs && n ==0 -> Right v
--                                            | otherwise -> Left $ unwords ["Partial decoding, left over data:",prettyLBS bs,show n]

--unflatRaw :: Flat a => L.ByteString -> Either String (a, L.ByteString, Int)
--unflatRaw bs = runPartialGet decode bs 0

unflatChk :: Flat a => L.ByteString -> Decoded a
unflatChk bs = case unflatPart bs of
                 Left e -> Left e
                 Right (v,bs,n) | L.null bs -> Right v -- && n ==0
                                | otherwise -> Left $ unwords $ if n == 0
                                                                then ["Partial decoding, left over data:",prettyLBS bs]
                                                                else ["Partial decoding, left over data:",prettyLBS bs,"minus",show n,"bits"]


unflatPart :: Flat a => L.ByteString -> Either String (a, L.ByteString, Int)
unflatPart bs = runPartialGet decode bs 0


--unflatIncremental = Get.runGetIncremental
-- runGet decode

-- encoded :: Flat a => a -> Encoded a
-- encoded = Encoded . flat

-- -- TODO: detect left over data and give error
-- decoded :: Flat a => Encoded a -> Decoded a
-- decoded = unflat . bytes

-- -- |Encoded data, mainly useful to show data in a nicer way
-- newtype Encoded a = Encoded {bytes::L.ByteString} deriving Show

-- instance Pretty (Encoded a) where pPrint = text . prettyLBS . bytes

instance Pretty L.ByteString where pPrint = text . prettyLBS

prettyLBS :: L.ByteString -> String
prettyLBS = unwords . map prettyWord8 . L.unpack

instance Pretty Word8 where pPrint = text . prettyWord8

prettyWord8 :: Word8 -> String
prettyWord8 = printf "%08b"

type Decoded a = Either DeserializeFailure a

type DeserializeFailure = String

instance Exception DeserializeFailure
