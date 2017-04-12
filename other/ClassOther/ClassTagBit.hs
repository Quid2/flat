{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures,
    ScopedTypeVariables, Trustworthy, TypeOperators, TypeSynonymInstances,DefaultSignatures ,FlexibleContexts ,DeriveGeneric ,StandaloneDeriving ,NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Flat.Class (
  -- * The Flat class
  Flat(..)
  ,module GHC.Generics
  ) where

-- import           Data.Flat.Types
import           GHC.Generics
import Data.Binary.Bits.Get ( Get, getBool )
-- import           Data.Flat.Encoding (eBits)
import           Data.Bits (shiftL,(.|.))
import Data.Flat.Prim
import Data.Word

-- |Class of types that can be encoded/decoded
class Flat a where
    encode :: a -> Encoding

    default encode :: (Generic a, GFlat (Rep a)) => a -> Encoding
    encode = gencode . from

    decode :: Get a
    default decode :: (Generic a, GFlat (Rep a)) => Get a
    decode = ({-# SCC decode_to #-} to) `fmap` gget

-- encodeAsList :: Flat a => [a] -> Encoding
-- encodeAsList []     = eBits 1 (0::Word8) -- Word64)
-- encodeAsList (x:xs) = eBits 1 1 <> encode x <> encodeAsList xs

-- |Default implementation based on Generics
-- Adapted from the cereal/binary packages.
class GFlat f where
    gencode :: f t -> Encoding
    gget :: Get (f t)

-- Type without constructors
instance GFlat V1 where
    gencode _ = error "unencodable"
    gget = undefined

-- Constructor without arguments
instance GFlat U1 where
    gencode U1 = mempty
    {-# INLINE  gencode #-}

    gget = pure U1
    {-# INLINE  gget #-}

-- Metadata (constructor name, etc)
instance GFlat a => GFlat (M1 i c a) where
    gencode = gencode . unM1
    {-# INLINE  gencode #-}

    gget = M1 <$> gget
    {-# INLINE  gget #-}

-- Product: constructor with parameters
instance (GFlat a, GFlat b) => GFlat (a :*: b) where
  gencode (x :*: y) = gencode x <> gencode y
  {-# INLINE gencode #-}
  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GFlat (K1 i a) where
  gencode = encode . unK1
  {-# INLINE gencode #-}
  gget = K1 <$> decode
  {-# INLINE gget #-}

-- Build representation bit by bit
instance (GSum a, GSum b, GFlat a, GFlat b) => GFlat (a :+: b) where
  gencode x = ensureByte <> encodeBit x
  --gencode x = encodeBit x
  {-# INLINE gencode #-}

  gget = {-# SCC "gget" #-} do
    tag <- getBool
    if tag then R1 <$> gget else L1 <$> gget
  {-# INLINE gget #-}

class GSum f where
    encodeBit :: f a -> Encoding

instance (GSum a, GSum b, GFlat a, GFlat b) => GSum (a :+: b) where
    encodeBit s = case s of
                    L1 x -> eFalse <> encodeBit x
                    R1 x -> eTrue  <> encodeBit x
    {-# INLINE  encodeBit #-}


instance GFlat a => GSum (C1 c a) where
    encodeBit = gencode
    {-# INLINE  encodeBit #-}
