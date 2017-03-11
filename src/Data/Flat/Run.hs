{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Run(flat
                    ,unflat
                    ,flatRaw
                    ,unflatRaw
                    ,DeserializeFailure
                    ,Decoded
                    ) where

import           Control.Exception
import           Data.Flat.Decoder
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           qualified Data.Flat.Encoder as E
import           Data.Flat.Filler
import           Data.Flat.Pretty

-- |Encode byte-padded value.
flat :: Flat a => a -> L.ByteString
flat = flatRaw . postAligned

-- |Encode value, if values does not end on byte boundary, 0s padding is added.
flatRaw :: Flat a => a -> L.ByteString
-- flatRaw a = E.encoderLazy (encode a)
flatRaw a = E.encoderStrict (getSize $ postAligned a) (encode a)
-- flatRaw a = bitEncoder (2000) (encode a)

-- |Decode byte-padded value
unflat :: Flat a => L.ByteString -> Decoded a
unflat bs = postValue <$> unflatChkWith decode bs
-- unflat bs = (\(PostAligned v _) -> v) <$> unflatChkWith decode bs

unflatRaw :: Flat a => L.ByteString -> Decoded a
-- unflatRaw = unflatChkWith decode
unflatRaw bs = (\(v,_,_) -> v) <$> runGetLazy decode bs

unflatChkWith :: Get a -> L.ByteString -> Either String a
unflatChkWith dec bs = case runGetLazy dec bs of
                         Left e -> Left e
                         Right (v,bs,n) | L.null bs -> Right v -- && n ==0
                                        | otherwise -> Left $ unwords $ if n == 0
                                                                then ["Partial decoding, left over data:",prettyLBS bs]
                                                                else ["Partial decoding, left over data:",prettyLBS bs,"minus",show n,"bits"]

--unflatPartWith :: Get b -> L.ByteString -> Either String (b, L.ByteString, Int)
--unflatPartWith dec bs = runPartialGet dec bs 0
-- unflatPartWith dec bs = runGetLazy dec bs

-- unflatWith :: Get a -> L.ByteString -> Either String a
-- unflatWith dec bs = unflatChkWith (do
--                                       v <- dec
--                                       _::Filler <- decode
--                                       return v) bs

-- unflatRaw :: Flat a => L.ByteString -> Decoded a
-- unflatRaw = runGetOrFail decode


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

