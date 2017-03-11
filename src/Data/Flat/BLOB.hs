{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
module Data.Flat.BLOB (BLOB(..),blob,unblob,FlatEncoding(..),UTF8Encoding(..))where

import           Data.Flat.Class
import qualified Data.ByteString.Lazy as L
import           Control.DeepSeq
import Data.Flat.Instances

data UTF8Encoding = UTF8Encoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

data UTF16Encoding = UTF16Encoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

data NoEncoding = NoEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

data FlatEncoding = FlatEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- data String e = Text (PreAligned (e (Array Word8)))
-- data BLOB encoding = BLOB (PreAligned (encoding (Array Word8))) deriving (Typeable,Generic)
-- or simply, to avoid higher-order kinds:
-- data BLOB encoding = BLOB encoding (PreAligned (Array Word8)) deriving (Eq,Ord,Show,Generic,Flat)
-- data BLOB = BLOB (PreAligned (Array Word8))
-- data Encoded encoding = CLOB encoding BLOB


-- The encoding is embedded as a value in order to support encodings that might have multiple values/variations.
--data BLOB encoding = BLOB encoding (PreAligned (Array Word8))
data BLOB encoding = BLOB encoding L.ByteString
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

blob :: encoding -> L.ByteString -> BLOB encoding
--blob enc = BLOB enc . preAligned
blob = BLOB

unblob :: BLOB encoding -> L.ByteString
-- unblob (BLOB _ pa) = preValue pa
unblob (BLOB _ pa) = pa
