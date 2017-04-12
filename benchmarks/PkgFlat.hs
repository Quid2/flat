{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
module PkgFlat(PkgFlat(..),Flat,getSize,sd,serializeF,deserializeF,serlN2) where

import           Control.Exception
import           Data.ByteString.Lazy as L
import           Data.Flat
import Data.Text
import           Test.Data
import           Test.Data.Flat
import           Test.Data.Values
import           Types
import Data.Map(Map)
import Data.Sequence(Seq)

data PkgFlat a = PkgFlat a deriving (Eq,Show)

instance Arbitrary a => Arbitrary (PkgFlat a) where arbitrary = fmap PkgFlat arbitrary

instance Flat a => Serialize PkgFlat a where
  serialize (PkgFlat a) = serializeF a
  deserialize = (PkgFlat <$>) . deserializeF
  pkg = PkgFlat
  unpkg (PkgFlat a) = a

--name = "decoderBinaryBits"
ver = show __GLASGOW_HASKELL__
-- ver = "801"
name = "decoderStrict"++ver
sd = ("encoderStrict"++ver, name, serializeF, deserializeF)

serializeF = flat
deserializeF =  either (Left . error) Right . unflat

-- instance Flat (Map Word32 N) where
--   size = sizeMap
--   encode = encodeMap
--   decode = decodeMap

-- instance Flat (Seq N) where
--   size = sizeSequence
--   encode = encodeSequence
--   decode = decodeSequence

--instance Flat N
--instance Flat a => Flat (List a)
--instance Flat a => Flat (Tree a)
instance Flat Car
instance Flat Acceleration
instance Flat Consumption
instance Flat CarModel
instance Flat OptionalExtra
instance Flat Engine


-- Car components
-- instance {-# OVERLAPPABLE #-} Flat a => Flat [a]
-- instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b)
-- instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c)

-- No speed up?
instance {-# OVERLAPPING #-} Flat [Word]
instance {-# OVERLAPPING #-} Flat [Bool]
instance {-# OVERLAPPING #-} Flat [Acceleration]
instance {-# OVERLAPPING #-} Flat [Int32]
instance {-# OVERLAPPING #-} Flat [OptionalExtra]
instance {-# OVERLAPPING #-} Flat [Consumption]
instance {-# OVERLAPPING #-} Flat [(OctaneRating,[Acceleration])]
instance {-# OVERLAPPING #-} Flat (OctaneRating,[Acceleration])

instance Flat (Tree ((N, N, N), (N, N, N), (N, N, N)))
-- instance Flat ((N, N, N), (N, N, N), (N, N, N))
instance Flat (N, N, (N, N, (N, (), N)))
instance Flat (Bool, (Bool, Bool), ((Bool, Bool, Bool), (Bool, Bool, Bool)))
instance Flat (Word, Word, (Word, Word))
instance Flat (Word, Word)
instance Flat ([Char], Text)
instance Flat ([Char], UTF8Text)
instance Flat ([Char], [Char])
instance Flat (Bool,Bool)
instance Flat ([Char], Tree (N, N, N))
instance Flat (N, N, (N, (), N))
instance Flat ((Bool, Bool, Bool), (Bool, Bool, Bool))
instance Flat (Bool, Bool, Bool)
instance Flat (N, (), N)

instance  {-# OVERLAPPING #-}  Flat (Tree N)
instance {-# OVERLAPPABLE #-} Flat a => Flat (Tree a)
instance {-# OVERLAPPING #-} Flat (Tree (N,N,N))
instance {-# OVERLAPPING #-} Flat [N]
instance {-# OVERLAPPING #-} Flat (N,N,N)
instance {-# OVERLAPPING #-} Flat (Word,Word8,Word16,Word32,Word64)

s = L.unpack $ flat $ lN2

serlN2 = L.pack [5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,129]
