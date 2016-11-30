{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Types(Serialize(..),ser,des
            ,SomeException
            ,Arbitrary(..)
            ,Int8,Int16,Int32,Int64
            ,Word,Word8,Word16,Word32,Word64
            ) where

import           Control.Exception
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L (ByteString, pack, unpack)
import           Data.Int
import           Data.Word
import           Test.QuickCheck
-- import qualified Quid2.HRT.Prelude as P

{-
class Serialize a where
  serialize :: a -> L.ByteString
  deserialize :: L.ByteString -> Either SomeException a
-}

class Serialize c a where
  serialize :: c a -> L.ByteString
  deserialize :: L.ByteString -> Either SomeException (c a)
  pkg :: a -> c a
  unpkg :: c a -> a


-- data Serializable = forall a. Serialize a => SZ a
-- instance

ser :: Serialize c a => c a -> [Word8]
ser = L.unpack . serialize

des :: Serialize c a => [Word8] -> Either SomeException (c a)
des = deserialize . L.pack
