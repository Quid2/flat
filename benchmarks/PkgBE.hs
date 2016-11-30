{- Binary-experiment test -}
{-# LANGUAGE PackageImports ,FlexibleInstances ,MultiParamTypeClasses#-}
module PkgBE(BEPkg(..)) where

import Data.Word
import Control.Exception
import Data.Monoid
import Control.Applicative

import qualified "binary-experiment"  Data.Binary.Serialize as BE (serialize,deserializeOrFail)
import "binary-experiment" Data.Binary.Serialize
-- import "binary-experiment-0.1.0.0" Data.Binary.Serialize.Class -- as BE  -- hiding (encode,decode)
import "binary-experiment" Data.Binary.Serialize.Encode
import "binary-experiment" Data.Binary.Serialize.Decode

import Types
import Test.Data

-- t = let Encoding e = BE.encode (33::Int32) in e OutStreamEnd

data BEPkg a = BEPkg a deriving (Eq,Show)

instance Arbitrary a => Arbitrary (BEPkg a) where arbitrary = fmap BEPkg arbitrary

{-
instance Binary a => Serialize (BEPkg a) where
  serialize (BEPkg a) = BE.serialize $ a
  deserialize = either (Left . toException) (Right . BEPkg) . BE.deserializeOrFail
-}

instance Binary a => Serialize BEPkg a where
  serialize (BEPkg a) =BE.serialize a
  deserialize = either (Left . toException) (Right . BEPkg) . BE.deserializeOrFail
  pkg = BEPkg
  unpkg (BEPkg a) = a


instance Binary a => Binary (List a) where
  encode (N) = encodeCtr0 1
  encode (C v l) = encodeCtr2 2 v l

  decode = do
     (t,l) <- decodeCtrTag
     case t of
       1 -> decodeCtrBody0 l N
       2 -> decodeCtrBody2 l C


instance Binary a => Binary (Tree a) where
  encode (Leaf a) = encodeCtr1 1 a
  encode (Node n1 n2) = encodeCtr2 2 n1 n2

  decode = do
     (t,l) <- decodeCtrTag
     case t of
       1 -> decodeCtrBody1 l Leaf
       2 -> decodeCtrBody2 l Node

{-# INLINE encodeCtr0 #-}
{-# INLINE encodeCtr1 #-}
{-# INLINE encodeCtr2 #-}
{-# INLINE encodeCtr3 #-}
{-# INLINE encodeCtr4 #-}
{-# INLINE encodeCtr6 #-}
{-# INLINE encodeCtr7 #-}

instance Binary N where
    -- 71 us
    encode One = word 0
    encode Two = word 1
    encode Three = word 2
    encode Four = word 3
    encode Five = word 4
    -- 77 us
    -- encode n = int (fromEnum n)
    -- 87 us
    -- encode = int . fromEnum
    decode = toEnum <$> expectInt

-- correct?
instance Binary () where
    encode _ = word 0
    decode = const () <$> expectInt

encodeCtr0 :: Word -> Encoding
encodeCtr1 :: Binary a => Word -> a -> Encoding
encodeCtr2 :: (Binary a, Binary b) => Word -> a -> b -> Encoding

encodeCtr0 n     = beginListLen 1 <> encode (n :: Word)
encodeCtr1 n a   = beginListLen 2 <> encode (n :: Word) <> encode a
encodeCtr2 n a b = beginListLen 3 <> encode (n :: Word) <> encode a <> encode b
encodeCtr3 n a b c
                 = beginListLen 4 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c
encodeCtr4 n a b c d
                 = beginListLen 5 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d
encodeCtr6 n a b c d e f
                 = beginListLen 7 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
encodeCtr7 n a b c d e f g
                 = beginListLen 8 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
                      <> encode g

{-# INLINE decodeCtrTag #-}
{-# INLINE decodeCtrBody0 #-}
{-# INLINE decodeCtrBody1 #-}
{-# INLINE decodeCtrBody2 #-}
{-# INLINE decodeCtrBody3 #-}

decodeCtrTag = (\len tag -> (tag, len)) <$> expectListLen <*> expectTag

decodeCtrBody0 1 f = pure f
decodeCtrBody1 2 f = do x1 <- decode
                        return (f x1)
decodeCtrBody2 3 f = do x1 <- decode
                        x2 <- decode
                        return (f x1 x2)
decodeCtrBody3 4 f = do x1 <- decode
                        x2 <- decode
                        x3 <- decode
                        return (f x1 x2 x3)
