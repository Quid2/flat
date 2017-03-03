{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Run(flat,unflat,unflatWith
                    ,flatRaw,unflatRaw
                    ,DeserializeFailure
                    --,runGet
                    ,getBool,Get -- for dynamic decoding
                    ,Decoded
                    ) where

import           Control.Exception
import           Data.Binary.Bits.Get
-- -- -- import qualified Data.Binary.Get                as Get (Decoder (..), Get,
                                                        -- runGetIncremental)
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           qualified Data.Flat.Encoder as E   -- (lazyEncoder)
import           Data.Flat.Filler
import           Data.Flat.Pretty

-- |Encode byte-padded value.
flat :: Flat a => a -> L.ByteString
flat = flatRaw . postAligned

-- |Decode byte-padded value
unflat :: Flat a => L.ByteString -> Decoded a
unflat bs = (\(PostAligned v _) -> v) <$> unflatChkWith decode bs

unflatWith :: Get a -> L.ByteString -> Either String a
unflatWith dec bs = unflatChkWith (do
                                      v <- dec
                                      _::Filler <- decode
                                      return v) bs

-- |Encode value, if values does not end on byte boundary, 0s padding is added.
flatRaw :: Flat a => a -> L.ByteString
-- flatRaw a = E.encoderLazy (encode a)
flatRaw a = E.encoderStrict (getSize $ postAligned a) (encode a)
-- flatRaw a = bitEncoder (2000) (encode a)

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

-- unflatChk :: Flat a => L.ByteString -> Decoded a
unflatChkWith :: Get a -> L.ByteString -> Either String a
unflatChkWith dec bs = case unflatPartWith dec bs of
                         Left e -> Left e
                         Right (v,bs,n) | L.null bs -> Right v -- && n ==0
                                        | otherwise -> Left $ unwords $ if n == 0
                                                                then ["Partial decoding, left over data:",prettyLBS bs]
                                                                else ["Partial decoding, left over data:",prettyLBS bs,"minus",show n,"bits"]


-- unflatPart :: Flat a => L.ByteString -> Either String (a, L.ByteString, Int)
-- unflatPart bs = runPartialGet decode bs 0

unflatPartWith :: Get b -> L.ByteString -> Either String (b, L.ByteString, Int)
unflatPartWith dec bs = runPartialGet dec bs 0

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

-- newtype Bits8 = Bits8 {bits8::Word8}
-- instance Pretty Bits8 where pPrint = text . prettyWord8 . bits8


type Decoded a = Either DeserializeFailure a

type DeserializeFailure = String

instance Exception DeserializeFailure

