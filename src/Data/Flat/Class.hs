{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}

module Data.Flat.Class (
  -- * The Flat class
  Flat(..),csize,ccsize,maxSize
  ,module GHC.Generics
  ) where

-- import           Data.Flat.Types
import           Data.Binary.Bits.Get (Get, getBool)
import           GHC.Generics
-- import           Data.Flat.Encoding (eBits)
import           Data.Bits            (shiftL, (.|.))
import           Data.Flat.Encoder
import           Data.Word
import           Prelude              hiding (mempty)
--import Data.Flat.GSize
-- import Data.Store.Internal hiding(genericSize,decode,encode,size)
-- import           GHC.Generics
import           GHC.TypeLits
import           Data.Proxy

-- |Class of types that can be encoded/decoded
class Flat a where

    {-# INLINE encode #-}
    encode :: a -> Encoding

    default encode :: (Generic a, GFlat (Rep a)) => a -> Encoding
    -- default encode :: (Generic a, GEnkode (Rep a)) => a -> Encoding
    encode = genericEncode

    {-# INLINE decode #-}
    decode :: Get a
    default decode :: (Generic a, GFlat (Rep a)) => Get a
    decode = genericDecode

    -- size :: Size a
    -- default size :: (Generic a, GSize (Rep a)) => Size a
    -- size = genericSize

    {-# INLINE size #-}
    size :: a -> NumBits -> NumBits
    default size :: (Generic a, GSize (Rep a)) => a -> NumBits -> NumBits
    size = genericSize

{-# INLINE genericEncode #-}
genericEncode :: (GFlat (Rep a), Generic a) => a -> Encoding
genericEncode = gencode . from

{-# INLINE genericDecode #-}
genericDecode :: (GFlat (Rep b), Generic b) => Get b
genericDecode = to `fmap` gget

{-# INLINE genericSize #-}
genericSize :: (GSize (Rep a), Generic a) => a -> NumBits -> NumBits
genericSize !x !n = gsize n $ from x

-- NOTE: this is just for the value, does not include final filler
maxSize :: Flat a => a -> NumBits
maxSize a = size a 0

csize :: (t -> NumBits) -> t -> NumBits -> NumBits
csize f n = ccsize (f n)

ccsize :: NumBits -> NumBits -> NumBits
ccsize !n !s = n+s

class GEncoders f where
  gencoders :: f t -> ([Encoding] -> [Encoding]) -> ([Encoding] -> [Encoding])

instance {-# OVERLAPPABLE #-} GEncoders a => GEncoders (M1 i c a) where
    gencoders m !l = gencoders (unM1 m) l
    {-# INLINE gencoders #-}

instance {-# OVERLAPPING #-} GEncoders a => GEncoders (D1 i (C1 c a)) where
    gencoders x !l = gencoders (unM1 . unM1 $ x) l
    {-# INLINE gencoders #-}

-- Type without constructors
instance GEncoders V1 where
    gencoders _ _ = unused

-- Constructor without arguments
instance GEncoders U1 where
    gencoders U1 !l = l
    {-# INLINE gencoders #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GEncoders (K1 i a) where
  gencoders k !l = l . (gencode k:)
  {-# INLINE gencoders #-}

-- Product: constructor with parameters
instance (GEncoders a, GEncoders b) => GEncoders (a :*: b) where
  gencoders (x :*: y) !l = gencoders y (gencoders x l)
  {-# INLINE gencoders #-}

-- |Default implementation based on Generics
class GFlat f where
  gencode :: f t -> Encoding
  gget :: Get (f t)
  --gsize ::

-- Metadata (constructor name, etc)
instance {-# OVERLAPPABLE #-} GFlat a => GFlat (M1 i c a) where
    gencode = gencode . unM1
    {-# INLINE  gencode #-}

    gget = M1 <$> gget
    {-# INLINE  gget #-}

-- Special case, single constructor datatype
instance {-# OVERLAPPING #-} (GFlat a,GEncoders a) => GFlat (D1 i (C1 c a)) where
    --gencode x = encodersR $ gencodersR x []
    gencode x = encodersS $ gencoders x id []
    -- gencode = gencode . unM1
    {-# INLINE  gencode #-}

    gget = M1 <$> gget
    {-# INLINE  gget #-}

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

-- Product: constructor with parameters
instance (GFlat a, GFlat b) => GFlat (a :*: b) where
  gencode (x :*: y) = unused -- error "unused" -- gencode x <> gencode y
  {-# INLINE gencode #-}

  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GFlat (K1 i a) where
  gencode = encode . unK1
  {-# INLINE gencode #-}

  gget = K1 <$> decode
  {-# INLINE gget #-}

-- Build constructor representation as single tag
instance (GSum a, GSum b, GFlat a, GFlat b) => GFlat (a :+: b) where
  gencode = encodeBit 0 0
  {-# INLINE gencode #-}

  gget = {-# SCC "gget" #-} do
    tag <- getBool
    if tag then R1 <$> gget else L1 <$> gget
  {-# INLINE gget #-}

unused = error $ "Now, now, you could not possibly have meant this.."

class GSum f where
    encodeBit :: Word8 -> NumBits -> f a -> Encoding
    --encodeBit :: Word8 -> Int -> f a -> (Word8,Int)

instance (GSum a, GSum b, GFlat a, GFlat b) => GSum (a :+: b) where
    encodeBit !code !numBits s = case s of
                             L1 !x -> encodeBit ((code `shiftL` 1) .|. 0) (numBits+1) x
                             R1 !x -> encodeBit ((code `shiftL` 1) .|. 1) (numBits+1) x
    {-# INLINE  encodeBit #-}

instance (GFlat a ,GEncoders a) => GSum (C1 c a) where
  encodeBit !code !numBits x =  encodersS $ gencoders x (eBits numBits code:) []
  {-# INLINE  encodeBit #-}

class GEncodeSum f where
    enkodeBit :: Word8 -> NumBits -> f a -> Encoding

instance (GEncodeSum a, GEncodeSum b, GFlat a, GFlat b) => GEncodeSum (a :+: b) where
    enkodeBit !code !numBits s = case s of
                             L1 !x -> enkodeBit ((code `shiftL` 1) .|. 0) (numBits+1) x
                             R1 !x -> enkodeBit ((code `shiftL` 1) .|. 1) (numBits+1) x
    {-# INLINE  enkodeBit #-}


class GSize f where gsize :: NumBits -> f a -> NumBits

instance GSize f => GSize (M1 i c f) where
    gsize !n = gsize n . unM1
    {-# INLINE gsize #-}

-- Special case, single constructor datatype
-- instance GSize a => GSize (D1 i (C1 c a)) where
--     gsize = gsize . unM1 . unM1
--     {-# INLINE gsize #-}

-- Type without constructors
instance GSize V1 where
    gsize !n _ = n
    {-# INLINE gsize #-}

-- Constructor without arguments
instance GSize U1 where
    gsize !n _ = n
    {-# INLINE gsize #-}

instance Flat a => GSize (K1 i a) where
    gsize !n x = size (unK1 x) n
    {-# INLINE gsize #-}

instance (GSize a, GSize b) => GSize (a :*: b) where
    -- gsize !n (!x :*: (!y)) = (gsize $! (gsize n x)) y
    -- gsize !n (!x :*: (!y)) = gsize (gsize n x) y
    gsize !n (x :*: y) = gsize (gsize n x) y
    {-# INLINE gsize #-}

instance (NumConstructors (a :+: b) <= 255, GSizeSum 0 (a :+: b)) => GSize (a :+: b) where
    gsize n !x = gsizeSum n x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}

class KnownNat n => GSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: NumBits -> f a -> Proxy n -> NumBits

instance (GSizeSum (n + 1) a, GSizeSum (n + 1) b, KnownNat n)
         => GSizeSum n (a :+: b) where
    gsizeSum !n x _ = case x of
                        L1 !l -> gsizeSum n l (Proxy :: Proxy (n+1))
                        R1 !r -> gsizeSum n r (Proxy :: Proxy (n+1))
    --gsizeSum !n (L1 l) _ = gsizeSum n l (Proxy :: Proxy (n+1))
    --gsizeSum !n (R1 r) _ = gsizeSum n r (Proxy :: Proxy (n+1))
    {-# INLINE gsizeSum #-}

instance (GSize a, KnownNat n) => GSizeSum n (C1 c a) where
    {-# INLINE gsizeSum #-}
    gsizeSum !n !x _ = gsize (constructorSize + n) x
      where
        constructorSize = fromInteger (natVal (Proxy :: Proxy n))

type family NumConstructors (a :: * -> *) :: Nat where
    NumConstructors (C1 c a) = 1
    NumConstructors (x :+: y) = NumConstructors x + NumConstructors y

