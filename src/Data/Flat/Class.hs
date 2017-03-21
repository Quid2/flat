{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |Generics-based generation of Flat instances
module Data.Flat.Class (
  -- * The Flat class
  Flat(..)
  ,getSize
  ,module GHC.Generics
  ) where

import           Data.Flat.Decoder (Get, dBool)
import           Data.Flat.Encoder
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Prelude           hiding (mempty)

-- |Class of types that can be encoded/decoded
class Flat a where

    {-# INLINE encode #-}
    encode :: a -> Encoding
    default encode :: (Generic a, GEncode (Rep a)) => a -> Encoding
    encode = genericEncode

    {-# INLINE decode #-}
    decode :: Get a
    default decode :: (Generic a, GDecode (Rep a)) => Get a
    decode = genericDecode

    {-# INLINE size #-}
    size :: a -> NumBits -> NumBits
    default size :: (Generic a, GSize (Rep a)) => a -> NumBits -> NumBits
    size = genericSize

{-# INLINE genericEncode #-}
genericEncode :: (GEncode (Rep a), Generic a) => a -> Encoding
genericEncode = gencode . from

{-# INLINE genericDecode #-}
genericDecode :: (GDecode (Rep b), Generic b) => Get b
genericDecode = to `fmap` gget

{-# INLINE genericSize #-}
genericSize :: (GSize (Rep a), Generic a) => a -> NumBits -> NumBits
genericSize !x !n = gsize n $ from x

-- |Calculate the size in bits of the serialisation of the value
getSize :: Flat a => a -> NumBits
getSize a = size a 0

-- |Generic Encoder
class GEncode f where
  gencode :: f t -> Encoding

-- Metadata (constructor name, etc)
instance {-# OVERLAPPABLE #-} GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1
  {-# INLINE  gencode #-}

-- Special case, single constructor datatype
instance {-# OVERLAPPING #-} (GEncoders a) => GEncode (D1 i (C1 c a)) where
  gencode !x = encodersS $ gencoders x id []
  {-# INLINE  gencode #-}

-- Type without constructors
instance GEncode V1 where
  gencode _ = unused
  {-# INLINE  gencode #-}

-- Constructor without arguments
instance GEncode U1 where
  gencode U1 = mempty
  {-# INLINE  gencode #-}

-- Product: constructor with parameters
instance GEncode (a :*: b) where
  gencode _ = unused
  {-# INLINE gencode #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GEncode (K1 i a) where
  gencode = encode . unK1
  {-# INLINE gencode #-}

-- Build constructor representation as single tag
instance (NumConstructors (a :+: b) <= 255, GEncodeSum 0 0 (a :+: b)) => GEncode (a :+: b) where
  gencode x = gencodeSum x (Proxy :: Proxy 0) (Proxy :: Proxy 0)
  {-# INLINE gencode #-}


class GEncoders f where
  -- |Determine the list of encoders corresponding to a type
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
  gencoders k !l = l . (gencode k :)
  {-# INLINE gencoders #-}

-- Product: constructor with parameters
instance (GEncoders a, GEncoders b) => GEncoders (a :*: b) where
  gencoders (x :*: y) !l = gencoders y (gencoders x l)
  {-# INLINE gencoders #-}

class (KnownNat code, KnownNat numBits) =>
      GEncodeSum (numBits:: Nat) (code :: Nat) (f :: * -> *) where
  gencodeSum :: f a -> Proxy numBits -> Proxy code -> Encoding

instance (GEncodeSum (n+1) (m*2) a,GEncodeSum (n+1) (m*2+1) b, KnownNat n,KnownNat m)
         => GEncodeSum n m (a :+: b) where
    gencodeSum !x _ _ = case x of
                         L1 l -> gencodeSum l (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2))
                         R1 r -> gencodeSum r (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2+1))
    {-# INLINE gencodeSum #-}

instance (GEncoders a, KnownNat n,KnownNat m) => GEncodeSum n m (C1 c a) where
    {-# INLINE gencodeSum #-}
    gencodeSum !x _ _  = encodersS $ gencoders x (eBits numBits code:) []
      where
        numBits = fromInteger (natVal (Proxy :: Proxy n))
        code = fromInteger (natVal (Proxy :: Proxy m))

type family NumConstructors (a :: * -> *) :: Nat where
    NumConstructors (C1 c a) = 1
    NumConstructors (x :+: y) = NumConstructors x + NumConstructors y

-- Generic Decoding
class GDecode f where
  gget :: Get (f t)

-- Metadata (constructor name, etc)
instance GDecode a => GDecode (M1 i c a) where
    gget = M1 <$> gget
    {-# INLINE  gget #-}

-- Type without constructors
instance GDecode V1 where
    gget = unused
    {-# INLINE  gget #-}

-- Constructor without arguments
instance GDecode U1 where
    gget = pure U1
    {-# INLINE  gget #-}

-- Product: constructor with parameters
instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GDecode (K1 i a) where
  gget = K1 <$> decode
  {-# INLINE gget #-}

-- Build constructor representation as single tag
instance (GDecode a, GDecode b) => GDecode (a :+: b) where
  gget = do
    !tag <- dBool
    !r <- if tag then R1 <$> gget else L1 <$> gget
    return r
  {-# INLINE gget #-}

-- |Calculate the size of a value in bits
class GSize f where gsize :: NumBits -> f a -> NumBits

instance GSize f => GSize (M1 i c f) where
    gsize !n = gsize n . unM1
    {-# INLINE gsize #-}

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
    gsize !n (x :*: y) = gsize (gsize n x) y
    {-# INLINE gsize #-}

instance (NumConstructors (a :+: b) <= 255, GSizeSum 0 (a :+: b)) => GSize (a :+: b) where
    gsize !n x = gsizeSum n x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}

class KnownNat n => GSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: NumBits -> f a -> Proxy n -> NumBits

instance (GSizeSum (n + 1) a, GSizeSum (n + 1) b, KnownNat n)
         => GSizeSum n (a :+: b) where
    gsizeSum !n x _ = case x of
                        L1 !l -> gsizeSum n l (Proxy :: Proxy (n+1))
                        R1 !r -> gsizeSum n r (Proxy :: Proxy (n+1))
    {-# INLINE gsizeSum #-}

instance (GSize a, KnownNat n) => GSizeSum n (C1 c a) where
    {-# INLINE gsizeSum #-}
    gsizeSum !n !x _ = gsize (constructorSize + n) x
      where
        constructorSize = fromInteger (natVal (Proxy :: Proxy n))

unused :: forall a . a
unused = error $ "Now, now, you could not possibly have meant this.."
