{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |Generics-based generation of Flat instances
module Data.Flat.Class
  (
  -- * The Flat class
    Flat(..)
  , getSize
  , module GHC.Generics
  )
where

import           Data.Flat.Decoder              ( Get
                                                , dBool
                                                )
import           Data.Flat.Encoder
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Prelude                 hiding ( mempty )
-- import Data.Flat.Generic.Decode(genericDecode)
import           Data.Word
import           Data.Bits

-- import GHC.Magic(inline)

-- |Class of types that can be encoded/decoded
class Flat a where

    {-# INLINE encode #-}
    encode :: a -> Encoding
    -- default encode :: (Generic a, GEncode (Rep a)) => a -> Encoding
    default encode :: (Generic a, GEnkode (Rep a)) => a -> Encoding
    encode = genericEncode

    {-# INLINE decode #-}
    decode :: Get a
    default decode :: (Generic a, GDecode (Rep a)) => Get a
    decode = genericDecode

    {-# INLINE size #-}
    size :: a -> NumBits -> NumBits
    default size :: (Generic a, GSize (Rep a)) => a -> NumBits -> NumBits
    size = genericSize

{-# INLINE genericSize #-}
genericSize :: (GSize (Rep a), Generic a) => a -> NumBits -> NumBits
genericSize !x !n = gsize n $ from x
-- genericSize = undefined

-- |Calculate the maximum size in bits of the serialisation of the value
getSize :: Flat a => a -> NumBits
getSize a = size a 0


{-# INLINE genericDecode #-}
genericDecode :: (GDecode (Rep b), Generic b) => Get b
genericDecode = to `fmap` gget
-- genericDecode = undefined

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
    -- R1 <$> gget
  {-# INLINE gget #-}


{-# INLINE genericEncode #-}
-- genericEncode :: (GEncode (Rep a), Generic a) => a -> Encoding
-- genericEncode = gencode . from

genericEncode :: (GEnkode (Rep a), Generic a) => a -> Encoding
genericEncode = genkode . from

-- genericEncode = undefined

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
  --gencode = inline encode . unK1
  gencode = encode . unK1
  {-# INLINE gencode #-}

-- Build constructor representation as single tag
instance (NumConstructors (a :+: b) <= 256, GEncodeSum 0 0 (a :+: b)) => GEncode (a :+: b) where
  gencode x = gencodeSum x (Proxy :: Proxy 0) (Proxy :: Proxy 0)
  {-# INLINE gencode #-}


class GEncoders f where
  -- |Determine the list of encoders corresponding to a type
  gencoders :: f t -> ([Encoding] -> [Encoding]) -> ([Encoding] -> [Encoding])

instance {-# OVERLAPPABLE #-} GEncoders a => GEncoders (M1 i c a) where
    gencoders m !l = gencoders (unM1 m) l
    {-# INLINE gencoders #-}

-- Special case, single constructor datatype
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

class GEnkode f where genkode :: f a -> Encoding

instance {-# OVERLAPPABLE #-} GEnkode f => GEnkode (M1 i c f) where
      genkode = genkode . unM1
      {-# INLINE genkode #-}

  -- Special case, single constructor datatype
instance {-# OVERLAPPING #-} GEnkode a => GEnkode (D1 i (C1 c a)) where
      genkode = genkode . unM1 . unM1
      {-# INLINE genkode #-}

  -- Type without constructors
instance GEnkode V1 where
      genkode = unused
      {-# INLINE genkode #-}

  -- Constructor without arguments
instance GEnkode U1 where
      genkode U1 = mempty
      {-# INLINE genkode #-}

instance Flat a => GEnkode (K1 i a) where
      genkode = encode . unK1
      {-# INLINE genkode #-}

instance (GEnkode a, GEnkode b) => GEnkode (a :*: b) where
      --genkode (!x :*: (!y)) = genkode x <++> genkode y
      genkode (x :*: y) = genkode x <> genkode y
      {-# INLINE genkode #-}

instance (NumConstructors (a :+: b) <= 256, GEnkodeSum (a :+: b)) => GEnkode (a :+: b) where
--      genkode x = genkodeSum x (Proxy :: Proxy 0) (Proxy :: Proxy 0)
      genkode x = genkodeSum 0 0 x
      -- genkode x = genkodeSum x 0 0
      {-# INLINE genkode #-}


-- |Encode sum (VERY SLOW AT COMPILATION TIME)
-- class (KnownNat code, KnownNat numBits) => GEnkodeSum (numBits:: Nat) (code :: Nat) (f :: * -> *) where
--    genkodeSum :: f a -> Proxy numBits -> Proxy code -> Encoding

-- instance (GEnkodeSum (n+1) (m*2) a,GEnkodeSum (n+1) (m*2+1) b, KnownNat n,KnownNat m) => GEnkodeSum n m (a :+: b) where
--     {-# INLINE genkodeSum #-}
--     genkodeSum !x _ _ = case x of
--                          L1 l -> genkodeSum l (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2))
--                          R1 r -> genkodeSum r (Proxy :: Proxy (n+1)) (Proxy :: Proxy (m*2+1))


-- instance (GEnkode a, KnownNat n,KnownNat m) => GEnkodeSum n m (C1 c a) where
--     {-# INLINE genkodeSum #-}
--     genkodeSum !x _ _  = eBits numBits code <> genkode x
--        where
--         numBits = fromInteger (natVal (Proxy :: Proxy n))
--         code = fromInteger (natVal (Proxy :: Proxy m))

class GEnkodeSum f where
  genkodeSum :: Word8 -> NumBits -> f a -> Encoding

instance (GEnkodeSum a, GEnkodeSum b) => GEnkodeSum (a :+: b) where
  genkodeSum !code !numBits s = case s of
                           L1 !x -> genkodeSum ((code `unsafeShiftL` 1)) (numBits+1) x
                           R1 !x -> genkodeSum ((code `unsafeShiftL` 1) .|. 1) (numBits+1) x
  {-# INLINE  genkodeSum #-}

instance GEnkode a => GEnkodeSum (C1 c a) where
  genkodeSum !code !numBits x = eBits numBits code <> genkode x
  {-# INLINE  genkodeSum #-}



-- class GEnkodeSum f where
--           genkodeSum :: f a -> Word8 -> NumBits  -> Encoding

-- instance (GEnkodeSum a, GEnkodeSum b) => GEnkodeSum (a :+: b) where
--           genkodeSum s !code !numBits = case s of
--                                    L1 !x -> genkodeSum x ((code `unsafeShiftL` 1)) (numBits+1)
--                                    R1 !x -> genkodeSum x ((code `unsafeShiftL` 1) .|. 1) (numBits+1)
--           {-# INLINE  genkodeSum #-}

-- instance GEnkode a => GEnkodeSum (C1 c a) where
--         genkodeSum x !code !numBits = eBits numBits code <> genkode x
--         {-# INLINE  genkodeSum #-}        



-- |Encode sum
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


-- |Calculate number of constructors
type family NumConstructors (a :: * -> *) :: Nat where
    NumConstructors (C1 c a) = 1
    NumConstructors (x :+: y) = NumConstructors x + NumConstructors y

type family ConsSize (a :: * -> *) :: Nat where
      ConsSize (C1 c a) = 0
      ConsSize (x :+: y) = 1 + Max (ConsSize x) (ConsSize y)

type family Max (n :: Nat) (m :: Nat) :: Nat where
   Max n m  = If (n <=? m) m n

type family If c (t::Nat) (e::Nat) where
    If 'True  t e = t
    If 'False t e = e

-- Proxy :: Proxy (ConsSize ). from 

-- class GConsSize (f :: * -> *) where gConsSize :: f a -> NumBits

-- instance GConsSize V1 where
--   gConsSize _ = 0
--   {-# INLINE gConsSize #-}

-- instance  GConsSize (C1 c a)  where gConsSize _ = 0

--instance  GConsSize f  where gConsSize _ =  fromInteger (natVal (Proxy :: Proxy 3)) -- (ConsSize f)))

-- instance  KnownNat (ConsSize f) => GConsSize f  where gConsSize _ =  fromInteger (natVal (Proxy :: (ConsSize f)))

-- instance (ConsSize f a) => GConsSize (C1 c a)  where gConsSize = 0 -- fromInteger (natVal (Proxy :: Proxy (ConsSize a)))

-- instance GConsSize f where gConsSize m = fromInteger (natVal (Proxy :: Proxy (ConsSize m)))


-- -- |Calculate size in bits of constructor
-- class KnownNat n => GConsSize (n :: Nat) (f :: * -> *) where gConsSize :: Proxy n -> NumBits

-- instance (gConsSize (n + 1) a, GConsSize (n + 1) b, KnownNat n)
--          => GConsSize n (a :+: b) where
--     gConsSize !n _ = case x of
--                         L1 !l -> gConsSize n l (Proxy :: Proxy (n+1))
--                         R1 !r -> gConsSize n r (Proxy :: Proxy (n+1))
--     {-# INLINE gConsSize #-}

-- instance (GSize a, KnownNat n) => GConsSize n (C1 c a) where
--     {-# INLINE gConsSize #-}
--     gConsSize !n !x _ = gsize (constructorSize + n) x
--       where
--         constructorSize :: NumBits
        -- constructorSize = fromInteger (natVal (Proxy :: Proxy n))





-- |Calculate the maximum number of bits required for the serialisation of a value
-- Implemented as a function that adds the maximum size to a running total
class GSize f where gsize :: NumBits -> f a -> NumBits

-- Skip metadata
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

-- Skip metadata    
instance Flat a => GSize (K1 i a) where
    gsize !n x = size (unK1 x) n
    {-# INLINE gsize #-}

instance (GSize a, GSize b) => GSize (a :*: b) where
    gsize !n (x :*: y) = gsize (gsize n x) y
    {-# INLINE gsize #-}

-- instance (NumConstructors (a :+: b) <= 256, GSizeSum 0 (a :+: b)) => GSize (a :+: b) where
-- instance (GSizeSum 0 (a :+: b)) => GSize (a :+: b) where 
--    gsize !n x = gsizeSum n x (Proxy :: Proxy 0) 
--    {-# INLINE gsize #-}

instance (GSizeSum (a :+: b)) => GSize (a :+: b) where
  gsize !n x = gsizeSum n x
  {-# INLINE gsize #-}

-- |Calculate size in bits of constructor
-- class KnownNat n => GSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: NumBits -> f a -> Proxy n -> NumBits

-- instance (GSizeSum (n + 1) a, GSizeSum (n + 1) b, KnownNat n)
--          => GSizeSum n (a :+: b) where
--     gsizeSum !n x _ = case x of
--                         L1 !l -> gsizeSum n l (Proxy :: Proxy (n+1))
--                         R1 !r -> gsizeSum n r (Proxy :: Proxy (n+1))
--     {-# INLINE gsizeSum #-}

-- instance (GSize a, KnownNat n) => GSizeSum n (C1 c a) where
--     {-# INLINE gsizeSum #-}
--     gsizeSum !n !x _ = gsize (constructorSize + n) x
--       where
--         constructorSize :: NumBits
--         constructorSize = fromInteger (natVal (Proxy :: Proxy n))

-- Calculate size in bits of constructor
-- vs proxy implementatio: similar compilation time but much better run times (at least for Tree N, -70%)
class GSizeSum (f :: * -> *) where gsizeSum :: NumBits -> f a ->  NumBits

instance (GSizeSum a, GSizeSum b)
         => GSizeSum (a :+: b) where
    gsizeSum !n x = case x of
                        L1 !l -> gsizeSum (n+1) l
                        R1 !r -> gsizeSum (n+1) r
    {-# INLINE gsizeSum #-}

instance (GSize a) => GSizeSum (C1 c a) where
    {-# INLINE gsizeSum #-}
    gsizeSum !n !x = gsize n x


unused :: forall a . a
unused = error $ "Now, now, you could not possibly have meant this.."
