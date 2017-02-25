{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
module PkgStore(PkgStore(..)
               --,serlN2
               ,Store,getSize
               ,sd) where -- Store,serializeF,deserializeF) where

import           Control.Exception
import           Data.ByteString   as B
import           Data.ByteString.Lazy   as L
import           Data.Store
import           Test.Data
import           Test.Data.Values
import Types

-- | Get the number of bytes needed to store the given value. See
-- 'size'.
getSize :: Store a => a -> Int
getSize = getSizeWith size
{-# INLINE getSize #-}

-- | Given a 'Size' value and a value of the type @a@, returns its 'Int'
-- size.
getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n
{-# INLINE getSizeWith #-}

data PkgStore a = PkgStore a deriving (Eq,Show)

instance Arbitrary a => Arbitrary (PkgStore a) where arbitrary = fmap PkgStore arbitrary

instance Store a => Serialize PkgStore a where
  serialize (PkgStore a) = serializeF a
  deserialize =  (PkgStore <$>) . deserializeF
  pkg = PkgStore
  unpkg (PkgStore a) = a

sd = ("store3",serializeF,deserializeF)
serializeF = L.fromStrict . encode
deserializeF =  either (Left . error . show) Right . decode . L.toStrict

instance Store Various
instance Store N
instance Store a => Store (List a)
instance Store Car
instance Store Acceleration
instance Store Consumption
instance Store CarModel
instance Store OptionalExtra
instance Store Engine
-- instance Store a => Store (Tree a)
-- Specialised instances
instance {-# OVERLAPPABLE #-} Store a => Store (Tree a)

instance {-# OVERLAPPING #-} Store (Tree N)
instance {-# OVERLAPPING #-} Store (Tree (N,N,N))
--instance {-# OVERLAPPING #-} Store [N]
instance {-# OVERLAPPING #-} Store (N,N,N)

s = B.unpack $ encode $ lN2

f = B.unpack $ encode $ (22.33::Float,44.55::Double)


