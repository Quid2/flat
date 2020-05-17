{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
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
module Flat.Class
  (
  -- * The Flat class
    Flat(..)
  , getSize
  , module GHC.Generics
  )
where

import           Data.Bits
import           Flat.Decoder
import           Flat.Encoder
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Prelude           hiding (mempty)
-- import Data.Proxy
-- External and Internal inlining
#define INL 2
-- Internal inlining
-- #define INL 1
-- No inlining
-- #define INL 0

#if INL == 1
import           GHC.Exts          (inline)
#endif

-- import           Data.Proxy

-- |Calculate the maximum size in bits of the serialisation of the value
getSize :: Flat a => a -> NumBits
getSize a = size a 0

-- |Class of types that can be encoded/decoded
class Flat a where
    -- |Return the encoding corrresponding to the value
    encode :: a -> Encoding
    default encode :: (Generic a, GEncode (Rep a)) => a -> Encoding
    encode = gencode . from

    -- |Decode a value
    decode :: Get a
    default decode :: (Generic a, GDecode (Rep a)) => Get a
    decode = to `fmap` gget

    -- |Add maximum size in bits of the value to the total count
    -- Used to calculated maximum buffer size before encoding 
    size :: a -> NumBits -> NumBits
    default size :: (Generic a, GSize (Rep a)) => a -> NumBits -> NumBits
    size !x !n = gsize n $ from x

#if INL>=2
    -- With these, generated code is optimised for specific data types (e.g.: Tree Bool will fuse the code of Tree and Bool)
    -- This can improve performance very significantly (up to 10X) but also increases compilation times.
    {-# INLINE size #-}
    {-# INLINE decode #-}
    {-# INLINE encode #-}
#elif INL == 1
#elif INL == 0
    {-# NOINLINE size #-}
    {-# NOINLINE decode #-}
    {-# NOINLINE encode #-}
#endif

-- |Generic Encoder
class GEncode f where gencode :: f a -> Encoding

instance {-# OVERLAPPABLE #-} GEncode f => GEncode (M1 i c f) where
      gencode = gencode . unM1
      {-# INLINE gencode #-}

  -- Special case, single constructor datatype
instance {-# OVERLAPPING #-} GEncode a => GEncode (D1 i (C1 c a)) where
      gencode = gencode . unM1 . unM1
      {-# INLINE gencode #-}

  -- Type without constructors
instance GEncode V1 where
      gencode = unused
      {-# INLINE gencode #-}

  -- Constructor without arguments
instance GEncode U1 where
      gencode U1 = mempty
      {-# INLINE gencode #-}

instance Flat a => GEncode (K1 i a) where
      {-# INLINE gencode #-}
#if INL == 1
      gencode x = inline encode (unK1 x)
#else
      gencode = encode . unK1
#endif

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
      --gencode (!x :*: (!y)) = gencode x <++> gencode y
      gencode (x :*: y) = gencode x <> gencode y
      {-# INLINE gencode #-}

instance (NumConstructors (a :+: b) <= 512,GEncodeSum (a :+: b)) => GEncode (a :+: b) where
-- instance (GEncodeSum (a :+: b)) => GEncode (a :+: b) where
      gencode = gencodeSum 0 0
      {-# INLINE gencode #-}

-- Constructor Encoding
class GEncodeSum f where
  gencodeSum :: Word16 -> NumBits -> f a -> Encoding

instance (GEncodeSum a, GEncodeSum b) => GEncodeSum (a :+: b) where
  gencodeSum !code !numBits s = case s of
                           L1 !x -> gencodeSum ((code `unsafeShiftL` 1)) (numBits+1) x
                           R1 !x -> gencodeSum ((code `unsafeShiftL` 1) .|. 1) (numBits+1) x
  {-# INLINE  gencodeSum #-}

instance GEncode a => GEncodeSum (C1 c a) where
  gencodeSum !code !numBits x = eBits16 numBits code <> gencode x
  {-# INLINE  gencodeSum #-}

-- |Generic Decoding
class GDecode f where
  gget :: Get (f t)

-- |Metadata (constructor name, etc)
instance GDecode a => GDecode (M1 i c a) where
    gget = M1 <$> gget
    {-# INLINE  gget #-}

-- |Type without constructors
instance GDecode V1 where
    gget = unused
    {-# INLINE  gget #-}

-- |Constructor without arguments
instance GDecode U1 where
    gget = pure U1
    {-# INLINE  gget #-}

-- |Product: constructor with parameters
instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

-- |Constants, additional parameters, and rank-1 recursion
instance Flat a => GDecode (K1 i a) where
#if INL == 1
  gget = K1 <$> inline decode
#else
  gget = K1 <$> decode
#endif
  {-# INLINE gget #-}


-- Different valid decoding setups
-- #define DEC_BOOLG
-- #define DEC_BOOL

-- #define DEC_BOOLG
-- #define DEC_BOOL
-- #define DEC_BOOL48

-- #define DEC_CONS
-- #define DEC_BOOLC
-- #define DEC_BOOL

-- #define DEC_CONS
-- #define DEC_BOOLC
-- #define DEC_BOOL
-- #define DEC_BOOL48

-- #define DEC_CONS

-- #define DEC_CONS
-- #define DEC_CONS48

#define DEC_CONS
#define DEC_CONS48
#define DEC_BOOLC
#define DEC_BOOL

#ifdef DEC_BOOLG
instance (GDecode a, GDecode b) => GDecode (a :+: b)
#endif

#ifdef DEC_BOOLC
-- Special case for data types with two constructors
instance {-# OVERLAPPING #-} (GDecode a,GDecode b) => GDecode (C1 m1 a :+: C1 m2 b)
#endif

#ifdef DEC_BOOL
  where
      gget = do
        -- error "DECODE2_C2"
        !tag <- dBool
        !r <- if tag then R1 <$> gget else L1 <$> gget
        return r
      {-# INLINE gget #-}
#endif

#ifdef DEC_CONS
-- | Data types with up to 512 constructors
-- Uses a custom constructor decoding state
-- instance {-# OVERLAPPABLE #-} (GDecodeSum (a :+: b),GDecode a, GDecode b) => GDecode (a :+: b) where
instance {-# OVERLAPPABLE #-} (NumConstructors (a :+: b) <= 512, GDecodeSum (a :+: b)) => GDecode (a :+: b) where
  gget = do
    cs <- consOpen
    getSum cs
  {-# INLINE gget #-}

-- |Constructor Decoder
class GDecodeSum f where
    getSum :: ConsState -> Get (f a)

#ifdef DEC_CONS48

-- Decode constructors in groups of 2 or 3 bits
-- Significantly reduce instance compilation time and slightly improve execution times
instance {-# OVERLAPPING #-} (GDecodeSum n1,GDecodeSum n2,GDecodeSum n3,GDecodeSum n4) => GDecodeSum ((n1 :+: n2) :+: (n3 :+: n4)) -- where -- getSum = undefined
      where
          getSum cs = do
            -- error "DECODE4"
            let (cs',tag) = consBits cs 2
            case tag of
              0 -> L1 . L1 <$> getSum cs'
              1 -> L1 . R1 <$> getSum cs'
              2 -> R1 . L1 <$> getSum cs'
              _ -> R1 . R1 <$> getSum cs'
          {-# INLINE getSum #-}

instance {-# OVERLAPPING #-} (GDecodeSum n1,GDecodeSum n2,GDecodeSum n3,GDecodeSum n4,GDecodeSum n5,GDecodeSum n6,GDecodeSum n7,GDecodeSum n8) => GDecodeSum (((n1 :+: n2) :+: (n3 :+: n4)) :+: ((n5 :+: n6) :+: (n7 :+: n8))) -- where -- getSum cs = undefined
     where
      getSum cs = do
        --error "DECODE8"
        let (cs',tag) = consBits cs 3
        case tag of
          0 -> L1 . L1 . L1 <$> getSum cs'
          1 -> L1 . L1 . R1 <$> getSum cs'
          2 -> L1 . R1 . L1 <$> getSum cs'
          3 -> L1 . R1 . R1 <$> getSum cs'
          4 -> R1 . L1 . L1 <$> getSum cs'
          5 -> R1 . L1 . R1 <$> getSum cs'
          6 -> R1 . R1 . L1 <$> getSum cs'
          _ -> R1 . R1 . R1 <$> getSum cs'
      {-# INLINE getSum #-}

instance {-# OVERLAPPABLE #-} (GDecodeSum a, GDecodeSum b) => GDecodeSum (a :+: b) where
#else
instance (GDecodeSum a, GDecodeSum b) => GDecodeSum (a :+: b) where
#endif

  getSum cs = do
    let (cs',tag) = consBool cs
    if tag then R1 <$> getSum cs' else L1 <$> getSum cs'
  {-# INLINE getSum #-}


instance GDecode a => GDecodeSum (C1 c a) where
    getSum (ConsState _ usedBits) = consClose usedBits >> gget
    {-# INLINE getSum #-}
#endif

#ifdef DEC_BOOL48
instance {-# OVERLAPPING #-} (GDecode n1,GDecode n2,GDecode n3,GDecode n4) => GDecode ((n1 :+: n2) :+: (n3 :+: n4)) -- where -- gget = undefined
  where
      gget = do
        -- error "DECODE4"
        !tag <- dBEBits8 2
        case tag of
          0 -> L1 <$> L1 <$> gget
          1 -> L1 <$> R1 <$> gget
          2 -> R1 <$> L1 <$> gget
          _ -> R1 <$> R1 <$> gget
      {-# INLINE gget #-}

instance {-# OVERLAPPING #-} (GDecode n1,GDecode n2,GDecode n3,GDecode n4,GDecode n5,GDecode n6,GDecode n7,GDecode n8) => GDecode (((n1 :+: n2) :+: (n3 :+: n4)) :+: ((n5 :+: n6) :+: (n7 :+: n8))) -- where -- gget = undefined
 where
  gget = do
    --error "DECODE8"
    !tag <- dBEBits8 3
    case tag of
      0 -> L1 <$> L1 <$> L1 <$> gget
      1 -> L1 <$> L1 <$> R1 <$> gget
      2 -> L1 <$> R1 <$> L1 <$> gget
      3 -> L1 <$> R1 <$> R1 <$> gget
      4 -> R1 <$> L1 <$> L1 <$> gget
      5 -> R1 <$> L1 <$> R1 <$> gget
      6 -> R1 <$> R1 <$> L1 <$> gget
      _ -> R1 <$> R1 <$> R1 <$> gget
  {-# INLINE gget #-}
#endif

-- |Calculate the number of bits required for the serialisation of a value
-- Implemented as a function that adds the maximum size to a running total
class GSize f where gsize :: NumBits -> f a -> NumBits

-- |Skip metadata
instance GSize f => GSize (M1 i c f) where
    gsize !n = gsize n . unM1
    {-# INLINE gsize #-}

-- |Type without constructors
instance GSize V1 where
    gsize !n _ = n
    {-# INLINE gsize #-}

-- |Constructor without arguments
instance GSize U1 where
    gsize !n _ = n
    {-# INLINE gsize #-}

-- |Skip metadata
instance Flat a => GSize (K1 i a) where
#if INL == 1
  gsize !n x = inline size (unK1 x) n
#else
  gsize !n x = size (unK1 x) n
#endif
  {-# INLINE gsize #-}

instance (GSize a, GSize b) => GSize (a :*: b) where
    gsize !n (x :*: y) = 
      let !n' = gsize n x
      in gsize n' y
      -- gsize (gsize n x) y
    {-# INLINE gsize #-}

-- Alternative 'gsize' implementations
#define SIZ_ADD
-- #define SIZ_NUM

-- #define SIZ_MAX
-- #define SIZ_MAX_VAL
-- #define SIZ_MAX_PROX

#ifdef SIZ_ADD
instance (GSizeSum (a :+: b)) => GSize (a :+: b) where
  gsize !n = gsizeSum n
#endif

#ifdef SIZ_NUM
instance (GSizeSum (a :+: b)) => GSize (a :+: b) where
  gsize !n x = n + gsizeSum 0 x
#endif

#ifdef SIZ_MAX
instance (GSizeNxt (a :+: b),GSizeMax (a:+:b)) => GSize (a :+: b) where
  gsize !n x = gsizeNxt (gsizeMax x + n) x
  {-# INLINE gsize #-}

-- |Calculate the maximum size of a class constructor (that might be one bit more than the size of some of its constructors)
#ifdef SIZ_MAX_VAL
class GSizeMax (f :: * -> *) where gsizeMax :: f a ->  NumBits

instance (GSizeMax f, GSizeMax g) => GSizeMax (f :+: g) where
    gsizeMax _ = 1 + max (gsizeMax (undefined::f a )) (gsizeMax (undefined::g a))
    {-# INLINE gsizeMax #-}

instance (GSize a) => GSizeMax (C1 c a) where
    {-# INLINE gsizeMax #-}
    gsizeMax _ = 0
#endif

#ifdef SIZ_MAX_PROX
-- instance (GSizeNxt (a :+: b),GSizeMax (a:+:b)) => GSize (a :+: b) where
--   gsize !n x = gsizeNxt (gsizeMax x + n) x
--   {-# INLINE gsize #-}


-- -- |Calculate size in bits of constructor
-- class KnownNat n => GSizeMax (n :: Nat) (f :: * -> *) where gsizeMax :: f a -> Proxy n -> NumBits

-- instance (GSizeMax (n + 1) a, GSizeMax (n + 1) b, KnownNat n) => GSizeMax n (a :+: b) where
--     gsizeMax !n x _ = case x of
--                         L1 !l -> gsizeMax n l (Proxy :: Proxy (n+1))
--                         R1 !r -> gsizeMax n r (Proxy :: Proxy (n+1))
--     {-# INLINE gsizeMax #-}

-- instance (GSize a, KnownNat n) => GSizeMax n (C1 c a) where
--     {-# INLINE gsizeMax #-}
--     gsizeMax !n !x _ = gsize (constructorSize + n) x
--       where
--         constructorSize :: NumBits
--         constructorSize = fromInteger (natVal (Proxy :: Proxy n))

-- class KnownNat (ConsSize f) => GSizeMax (f :: * -> *) where
--   gsizeMax :: f a ->  NumBits
--   gsizeMax _ = fromInteger (natVal (Proxy :: Proxy (ConsSize f)))

type family ConsSize (a :: * -> *) :: Nat where
      ConsSize (C1 c a) = 0
      ConsSize (x :+: y) = 1 + Max (ConsSize x) (ConsSize y)

type family Max (n :: Nat) (m :: Nat) :: Nat where
   Max n m  = If (n <=? m) m n

type family If c (t::Nat) (e::Nat) where
    If 'True  t e = t
    If 'False t e = e
#endif

-- |Calculate the size of a value, not taking in account its constructor
class GSizeNxt (f :: * -> *) where gsizeNxt :: NumBits -> f a ->  NumBits

instance (GSizeNxt a, GSizeNxt b) => GSizeNxt (a :+: b) where
    gsizeNxt n x = case x of
                        L1 !l-> gsizeNxt n l
                        R1 !r-> gsizeNxt n r
    {-# INLINE gsizeNxt #-}

instance (GSize a) => GSizeNxt (C1 c a) where
    {-# INLINE gsizeNxt #-}
    gsizeNxt !n !x = gsize n x
#endif

-- |Calculate size in bits of constructor
-- vs proxy implementation: similar compilation time but much better run times (at least for Tree N, -70%)
class GSizeSum (f :: * -> *) where gsizeSum :: NumBits -> f a ->  NumBits

instance (GSizeSum a, GSizeSum b)
         => GSizeSum (a :+: b) where
    gsizeSum !n x = case x of
                        L1 !l-> gsizeSum (n+1) l
                        R1 !r-> gsizeSum (n+1) r
    {-# INLINE gsizeSum #-}

instance (GSize a) => GSizeSum (C1 c a) where
    {-# INLINE gsizeSum #-}
    gsizeSum !n !x = gsize n x


-- |Calculate number of constructors
type family NumConstructors (a :: * -> *) :: Nat where
  NumConstructors (C1 c a) = 1
  NumConstructors (x :+: y) = NumConstructors x + NumConstructors y

unused :: forall a . a
unused = error "Now, now, you could not possibly have meant this.."
