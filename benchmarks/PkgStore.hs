{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
module PkgStore(PkgStore(..)
               --,serlN2
               ) where

import           Control.Exception
import           Data.ByteString   as B
import           Data.ByteString.Lazy   as L
import           Data.Store
import           Test.Data
import           Test.Data.Values
import Types

data PkgStore a = PkgStore a deriving (Eq,Show)

instance Arbitrary a => Arbitrary (PkgStore a) where arbitrary = fmap PkgStore arbitrary

instance Store a => Serialize PkgStore a where
  serialize (PkgStore a) = L.fromStrict $ encode a
  deserialize = either (Left . error . show) (Right . PkgStore) . decode . L.toStrict
  pkg = PkgStore
  unpkg (PkgStore a) = a

instance Store N
instance Store a => Store (List a)
instance Store a => Store (Tree a)
instance Store Car
instance Store Acceleration
instance Store Consumption
instance Store CarModel
instance Store OptionalExtra
instance Store Engine

s = B.unpack $ encode $ lN2

f = B.unpack $ encode $ (22.33::Float,44.55::Double)


