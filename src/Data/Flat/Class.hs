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
  Flat(..)
  -- ,Size(..),cmapSize
  ,getSize
  ,module GHC.Generics
  ) where

import           Data.Flat.Decoder (Get, dBool)
import           GHC.Generics
import           Data.Bits            (shiftL, (.|.))
import           Data.Flat.Encoder
import           Data.Word
import           Prelude              hiding (mempty)
import           GHC.TypeLits
import           Data.Proxy
-- import Data.Flat.Size

-- |Class of types that can be encoded/decoded
class Flat a where

    {-# INLINE encode #-}
    encode :: a -> Encoding

    default encode :: (Generic a, GEncode (Rep a)) => a -> Encoding
    -- default encode :: (Generic a, GEnkode (Rep a)) => a -> Encoding
    encode = genericEncode

    {-# INLINE decode #-}
    decode :: Get a
    default decode :: (Generic a, GDecode (Rep a)) => Get a
    decode = genericDecode

    {-# INLINE size #-}
    -- size :: Size a
    -- default size :: (Generic a, GSize (Rep a)) => Size a
    -- size = genericSize

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
-- genericSize :: (Generic a, GSize (Rep a)) => Size a
-- genericSize = contramapSize from gsize

genericSize :: (GSize (Rep a), Generic a) => a -> NumBits -> NumBits
genericSize !x !n = gsize n $ from x

-- NOTE: this is just for the value, does not include final filler
getSize :: Flat a => a -> NumBits
getSize a = size a 0

-- |Default implementation based on Generics
class GEncode f where
  gencode :: f t -> Encoding

-- Metadata (constructor name, etc)
instance {-# OVERLAPPABLE #-} GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1
  {-# INLINE  gencode #-}

-- Special case, single constructor datatype
instance {-# OVERLAPPING #-} (GEncode a,GEncoders a) => GEncode (D1 i (C1 c a)) where
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
instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode _ = unused
  {-# INLINE gencode #-}

-- Constants, additional parameters, and rank-1 recursion
instance Flat a => GEncode (K1 i a) where
  gencode = encode . unK1
  {-# INLINE gencode #-}

-- Build constructor representation as single tag
--instance (GSum a, GSum b, GEncode a, GEncode b) => GEncode (a :+: b) where
instance (NumConstructors (a :+: b) <= 255, GEnkodeSum 0 0 (a :+: b),GEncode a, GEncode b) => GEncode (a :+: b) where
  -- gencode = encodeBit 0 0
  gencode x = genkodeSum x (Proxy :: Proxy 0) (Proxy :: Proxy 0)
  {-# INLINE gencode #-}

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
  gencoders k !l = l . (gencode k :)
  {-# INLINE gencoders #-}

-- Product: constructor with parameters
instance (GEncoders a, GEncoders b) => GEncoders (a :*: b) where
  gencoders (x :*: y) !l = gencoders y (gencoders x l)
  {-# INLINE gencoders #-}


class GDecode f where
  gget :: Get (f t)

-- Metadata (constructor name, etc)
instance GDecode a => GDecode (M1 i c a) where
    gget = M1 <$> gget
    {-# INLINE  gget #-}

-- Type without constructors
instance GDecode V1 where
    gget = undefined
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
    if tag then R1 <$> gget else L1 <$> gget
  {-# INLINE gget #-}

class GSum f where
    encodeBit :: Word8 -> NumBits -> f a -> Encoding
    --encodeBit :: Word8 -> Int -> f a -> (Word8,Int)

instance (GSum a, GSum b) => GSum (a :+: b) where
    encodeBit !code !numBits !s = case s of
                             L1 !x -> encodeBit ((code `shiftL` 1) .|. 0) (numBits+1) x
                             R1 !x -> encodeBit ((code `shiftL` 1) .|. 1) (numBits+1) x
    {-# INLINE  encodeBit #-}

instance GEncoders a => GSum (C1 c a) where
  encodeBit !code !numBits !x =  encodersS $ gencoders x (eBits numBits code:) []
  {-# INLINE  encodeBit #-}

class (KnownNat code, KnownNat numBits) =>
      GEnkodeSum (numBits:: Nat) (code :: Nat) (f :: * -> *) where
  genkodeSum :: f a -> Proxy numBits -> Proxy code -> Encoding

instance (GEnkodeSum (n+1) (m*2) a,GEnkodeSum (n+1) (m*2+1) b, KnownNat n,KnownNat m)
         => GEnkodeSum n m (a :+: b) where
    genkodeSum !x _ _ = case x of
                         L1 l -> genkodeSum l (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2))
                         R1 r -> genkodeSum r (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2+1))
    {-# INLINE genkodeSum #-}

instance (GEncoders a, KnownNat n,KnownNat m) => GEnkodeSum n m (C1 c a) where
    {-# INLINE genkodeSum #-}
    -- genkodeSum !x !n !m = eBits numBits code <> genkode x
    genkodeSum !x _ _  = encodersS $ gencoders x (eBits numBits code:) []
      where
        numBits = fromInteger (natVal (Proxy :: Proxy n))
        code = fromInteger (natVal (Proxy :: Proxy m))


type family NumConstructors (a :: * -> *) :: Nat where
    NumConstructors (C1 c a) = 1
    NumConstructors (x :+: y) = NumConstructors x + NumConstructors y

{-
Calculating the size in bits of a value:

a) Dynamically add the sizes
b) As in the 'store' package, distinguish between ConstSize and VarSize,
   Unfortunately any data type with multiple constructors will be a VarSize.
   Main advantage is for structures with elements of fixed sizes that can calculate their size much more efficiently.

    b1) Pretend that Word/Int/Char have fixed sizes by attributing them the largest size they might use
    b2) Use actual sizes

|           | Simplicity | Speed   | Memory Use |
| a         | ++         |  ++     | ++         |
| b1        | =          |  +      | --         |
| b2        | =          |  -      | ++         |
-}

-- Simple version of GSize
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


-- Store-Like Size

-- -- | Info about a type's serialized length. Either the length is known
-- -- independently of the value, or the length depends on the value.
-- data Size a
--     = VarSize (a -> Int)
--     | ConstSize !Int
--     -- deriving Typeable

-- instance Show (Size a) where
--   show (ConstSize n) = unwords ["ConstSize",show n]
--   show (VarSize _) = "VarSize"

-- -- | Given a 'Size' value and a value of the type @a@, returns its 'Int'
-- -- size.
-- getSizeWith :: Size a -> a -> Int
-- getSizeWith (VarSize f) x = f x
-- getSizeWith (ConstSize n) _ = n
-- {-# INLINE getSizeWith #-}

-- -- | This allows for changing the type used as an input when the 'Size'
-- -- is 'VarSize'.
-- contramapSize :: (a -> b) -> Size b -> Size a
-- contramapSize f (VarSize g) = VarSize (g . f)
-- contramapSize _ (ConstSize n) = ConstSize n
-- {-# INLINE contramapSize #-}

-- -- | Create an aggregate 'Size' by providing functions to split the
-- -- input into two pieces, as well as 'Size' values to use to measure the
-- -- results.
-- --
-- -- If both of the input 'Size' values are 'ConstSize', the result is
-- -- 'ConstSize' and the functions will not be used.
-- combineSizeWith :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
-- combineSizeWith toA toB sizeA sizeB =
--     case (sizeA, sizeB) of
--         (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
--         (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
--         (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
--         (ConstSize n, ConstSize m) -> ConstSize (n + m)
-- {-# INLINE combineSizeWith #-}

-- -- | Adds a constant amount to a 'Size' value.
-- addSize :: Int -> Size a -> Size a
-- addSize x (ConstSize n) = ConstSize (x + n)
-- addSize x (VarSize f) = VarSize ((x +) . f)
-- {-# INLINE addSize #-}



-- -- | Get the number of bytes needed to store the given value. See
-- -- 'size'.
-- getSize :: Flat a => a -> Int
-- getSize = getSizeWith size
-- {-# INLINE getSize #-}

-- cmapSize :: forall a b. Flat b => (a -> b) -> Size a
-- cmapSize f = case size :: Size b of
--                VarSize g -> VarSize (g . f)
--                ConstSize n -> ConstSize n
-- {-# INLINE cmapSize #-}

-- combineSizes :: forall a b c. (Flat a, Flat b) => (c -> a) -> (c -> b) -> Size c
-- combineSizes toA toB =
--     case (size::Size a, size::Size b) of
--         (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
--         (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
--         (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
--         (ConstSize n, ConstSize m) -> ConstSize (n + m)
-- {-# INLINE combineSizes #-}

-- class GSize f where gsize :: Size (f a)

-- instance GSize f => GSize (M1 i c f) where
--     gsize = contramapSize unM1 gsize
--     {-# INLINE gsize #-}

-- -- Type without constructors
-- instance GSize V1 where
--     gsize = ConstSize 0
--     {-# INLINE gsize #-}

-- -- Constructor without arguments
-- instance GSize U1 where
--     gsize = ConstSize 0
--     {-# INLINE gsize #-}
 
-- instance Flat a => GSize (K1 i a) where
--     gsize = contramapSize unK1 size
--     {-# INLINE gsize #-}

-- instance (GSize a, GSize b) => GSize (a :*: b) where
--     gsize = combineSizeWith (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
--     {-# INLINE gsize #-}

-- instance GSizeSum 0 (a :+: b) => GSize (a :+: b) where
--     gsize = VarSize $ \x -> gsizeSum x (Proxy :: Proxy 0)
--     {-# INLINE gsize #-}

-- class KnownNat n => GSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: f a -> Proxy n -> NumBits

-- instance (GSizeSum (n+1) a, GSizeSum (n+1) b, KnownNat n) => GSizeSum n (a :+: b) where
--     gsizeSum !(L1 !l) _ = gsizeSum l (Proxy :: Proxy (n+1))
--     gsizeSum !(R1 !r) _ = gsizeSum r (Proxy :: Proxy (n+1))
--     {-# INLINE gsizeSum #-}

-- instance (GSize a, KnownNat n) => GSizeSum n (C1 c a) where
--     gsizeSum !x _ = getSizeWith (addSize constructorSize gsize) x
--       where
--         constructorSize :: Int
--         constructorSize = fromInteger (natVal (Proxy :: Proxy n))
--     {-# INLINE gsizeSum #-}

