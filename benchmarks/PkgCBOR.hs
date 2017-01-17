{- Binary-experiment test -}
{-# LANGUAGE PackageImports ,FlexibleInstances ,MultiParamTypeClasses#-}
module PkgCBOR(PkgCBOR(..)) where

import Data.Word
import Control.Exception
import Data.Monoid
import Control.Applicative

import qualified Data.Binary.Serialise.CBOR as C -- (serialise,deserialiseOrFail,Seriali)
import Data.Binary.Serialise.CBOR (Serialise(..))
import Data.Binary.Serialise.CBOR.Class
import Data.Binary.Serialise.CBOR.Encoding hiding (Tokens(..))
import Data.Binary.Serialise.CBOR.Decoding
import Data.Binary.Serialise.CBOR.Read
import Data.Binary.Serialise.CBOR.Write


import Types
import Test.Data

-- t = let Encoding e = C.encode (33::Int32) in e OutStreamEnd

data PkgCBOR a = PkgCBOR a deriving (Eq,Show)

instance Arbitrary a => Arbitrary (PkgCBOR a) where arbitrary = fmap PkgCBOR arbitrary

{-
instance Serialise a => Serialize (PkgCBOR a) where
  serialize (PkgCBOR a) = C.serialize $ a
  deserialize = either (Left . toException) (Right . PkgCBOR) . C.deserializeOrFail
-}

instance Serialise a => Serialize PkgCBOR a where
  serialize (PkgCBOR a) = C.serialise a
  deserialize = either (Left . toException) (Right . PkgCBOR) . C.deserialiseOrFail
  pkg = PkgCBOR
  unpkg (PkgCBOR a) = a

instance Serialise N where
    encode One = encodeCtr0 1
    encode Two = encodeCtr0 2
    encode Three = encodeCtr0 3
    encode Four = encodeCtr0 4
    encode Five = encodeCtr0 5

    decode = do
      (t,l) <- decodeCtrTag
      case t of
        1 -> decodeCtrBody0 l One
        2 -> decodeCtrBody0 l Two
        3 -> decodeCtrBody0 l Three
        4 -> decodeCtrBody0 l Four
        5 -> decodeCtrBody0 l Five

instance Serialise a => Serialise (List a) where
  encode (N) = encodeCtr0 1
  encode (C v l) = encodeCtr2 2 v l

  decode = do
     (t,l) <- decodeCtrTag
     case t of
       1 -> decodeCtrBody0 l N
       2 -> decodeCtrBody2 l C

instance Serialise a => Serialise (Tree a) where
  encode (Leaf a) = encodeCtr1 1 a
  encode (Node n1 n2) = encodeCtr2 2 n1 n2

  decode = do
     (t,l) <- decodeCtrTag
     case t of
       1 -> decodeCtrBody1 l Leaf
       2 -> decodeCtrBody2 l Node

{-

-- correct?
instance Serialise () where
    encode _ = word 0
    decode = const () <$> expectInt
-}

encodeCtr0 :: Word -> Encoding
encodeCtr1 :: Serialise a => Word -> a -> Encoding
encodeCtr2 :: (Serialise a, Serialise b) => Word -> a -> b -> Encoding

encodeCtr0 n     = encodeListLen 1 <> encode (n :: Word)
encodeCtr1 n a   = encodeListLen 2 <> encode (n :: Word) <> encode a
encodeCtr2 n a b = encodeListLen 3 <> encode (n :: Word) <> encode a <> encode b
encodeCtr3 n a b c
                 = encodeListLen 4 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c
encodeCtr4 n a b c d
                 = encodeListLen 5 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d
encodeCtr6 n a b c d e f
                 = encodeListLen 7 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
encodeCtr7 n a b c d e f g
                 = encodeListLen 8 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
                      <> encode g

{-# INLINE encodeCtr0 #-}
{-# INLINE encodeCtr1 #-}
{-# INLINE encodeCtr2 #-}
{-# INLINE encodeCtr3 #-}
{-# INLINE encodeCtr4 #-}
{-# INLINE encodeCtr6 #-}
{-# INLINE encodeCtr7 #-}

{-# INLINE decodeCtrTag #-}
{-# INLINE decodeCtrBody0 #-}
{-# INLINE decodeCtrBody1 #-}
{-# INLINE decodeCtrBody2 #-}
{-# INLINE decodeCtrBody3 #-}

decodeCtrTag = (\len tag -> (tag, len)) <$> decodeListLen <*> decodeWord

decodeCtrBody0 1 f = pure f
decodeCtrBody1 2 f = do x1 <- decode
                        return $! f x1
decodeCtrBody2 3 f = do x1 <- decode
                        x2 <- decode
                        return $! f x1 x2
decodeCtrBody3 4 f = do x1 <- decode
                        x2 <- decode
                        x3 <- decode
                        return $! f x1 x2 x3

{-# INLINE decodeSingleCtr0 #-}
{-# INLINE decodeSingleCtr1 #-}
{-# INLINE decodeSingleCtr2 #-}
{-# INLINE decodeSingleCtr3 #-}
{-# INLINE decodeSingleCtr4 #-}
{-# INLINE decodeSingleCtr6 #-}
{-# INLINE decodeSingleCtr7 #-}

decodeSingleCtr0 v f = decodeListLenOf 1 *> decodeWordOf v *> pure f
decodeSingleCtr1 v f = decodeListLenOf 2 *> decodeWordOf v *> pure f <*> decode
decodeSingleCtr2 v f = decodeListLenOf 3 *> decodeWordOf v *> pure f <*> decode <*> decode
decodeSingleCtr3 v f = decodeListLenOf 4 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode
decodeSingleCtr4 v f = decodeListLenOf 5 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode
decodeSingleCtr6 v f = decodeListLenOf 7 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode
decodeSingleCtr7 v f = decodeListLenOf 8 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance Serialise Car
instance Serialise Acceleration
instance Serialise Consumption
instance Serialise CarModel
instance Serialise OptionalExtra
instance Serialise Engine
instance Serialise Various
