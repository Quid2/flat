{-# LANGUAGE PackageImports #-}
module PkgT(PkgByte(..),PkgByte2(..),PkgBit(..),PkgBit2(..),PkgBit3(..)) where
--
import Data.Word
import Control.Exception
import Data.Monoid
import Control.Applicative
import Data.Binary.Serialize.Coder
import Types
import Test.Data

data PkgByte a = PkgByte a deriving (Eq,Show)
instance Arbitrary a => Arbitrary (PkgByte a) where arbitrary = fmap PkgByte arbitrary
instance Binary a => Serialize (PkgByte a) where
  serialize (PkgByte a) = encodeByte a
  deserialize = Right . PkgByte . decodeByte

data PkgByte2 a = PkgByte2 a deriving (Eq,Show)
instance Arbitrary a => Arbitrary (PkgByte2 a) where arbitrary = fmap PkgByte2 arbitrary
instance Binary a => Serialize (PkgByte2 a) where
  serialize (PkgByte2 a) = encodeByte2 a
  deserialize = Right . PkgByte2 . decodeByte

data PkgBit a = PkgBit a deriving (Eq,Show)
instance Arbitrary a => Arbitrary (PkgBit a) where arbitrary = fmap PkgBit arbitrary
instance Binary a => Serialize (PkgBit a) where
  serialize (PkgBit a) = encodeBit a
  deserialize = Right . PkgBit . decodeBit

data PkgBit2 a = PkgBit2 a deriving (Eq,Show)
instance Arbitrary a => Arbitrary (PkgBit2 a) where arbitrary = fmap PkgBit2 arbitrary
instance Binary a => Serialize (PkgBit2 a) where
  serialize (PkgBit2 a) = encodeBit2 a
  {-# INLINE serialize #-}
  deserialize = Right . PkgBit2 . decodeBit
  {-# INLINE deserialize #-}

data PkgBit3 a = PkgBit3 a deriving (Eq,Show)
instance Arbitrary a => Arbitrary (PkgBit3 a) where arbitrary = fmap PkgBit3 arbitrary
instance Binary a => Serialize (PkgBit3 a) where
  serialize (PkgBit3 a) = encodeBit3 a
  -- deserialize = Right . PkgBit3 . decodeBit3

{-
instance AsTerm a=> Serialize (PkgT a) where
  serialize (PkgT a) = encode a
  -- deserialize = either (Left . toException) (Right . PkgT) . BE.deserializeOrFail
-}
