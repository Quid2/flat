{-# LANGUAGE DeriveGeneric ,DeriveAnyClass #-}
module Data.Flat.Filler(Filler(..),fillerLength
                       ,PreAligned(..),preAligned,postAligned,PostAligned(..)
                       ) where

import           Data.Flat.Class
import           Data.Flat.Encoder
import           Control.DeepSeq
import           Data.Typeable

-- |A meaningless sequence of 0 bits terminated with a 1 bit (easier to implement than the reverse)
-- Useful to align an encoded value at byte/word boundaries.
data Filler = FillerBit Filler
            | FillerEnd
  deriving (Show, Eq, Ord, Typeable, Generic, NFData)

-- |Use a special encoding for the filler
instance Flat Filler where
  encode _ = eFiller
  size = sFiller

-- |A Post aligned value, a value followed by a filler
data PostAligned a = PostAligned { postValue :: a, postFiller :: Filler }
  deriving (Show, Eq, Ord, Typeable, Generic, NFData,Flat)

-- TO REMOVE
-- instance Flat a => Flat (PostAligned a) where size (PostAligned a _) = size a 8

-- |A Pre aligned value, a value preceded by a filler
data PreAligned a = PreAligned { preFiller :: Filler, preValue :: a }
  deriving (Show, Eq, Ord, Typeable, Generic, NFData, Flat)

-- |Length of a filler in bits
fillerLength :: Num a => Filler -> a
fillerLength FillerEnd = 1
fillerLength (FillerBit f) = 1 + fillerLength f

-- |Post align a value
postAligned :: a -> PostAligned a
postAligned a = PostAligned a FillerEnd

-- |Pre align a value
preAligned :: a -> PreAligned a
preAligned a = PreAligned FillerEnd a

