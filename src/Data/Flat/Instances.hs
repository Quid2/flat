{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving      ,CPP  #-}
{-# LANGUAGE UndecidableInstances ,MonoLocalBinds   #-}
-- {-# LANGUAGE IncoherentInstances    #-}

-- |Flat Instances for common, primitive and abstract data types for which instances cannot be automatically derived
module Data.Flat.Instances
  ( sizeMap
  , encodeMap
  , decodeMap
  , sizeSequence
  , encodeSequence
  , decodeSequence
  )
where
-- import Data.Function (trampoline)

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Short         as SBS
import           Data.Char
import           Data.Containers                ( ContainerKey
                                                , IsMap
                                                , MapValue
                                                , mapFromList
                                                , mapToList
                                                )
import           Data.Flat.Class
import           Data.Flat.Decoder
import           Data.Flat.Encoder
--import           Data.Flat.Size        (arrayBits)
-- import qualified Data.DList                    as DL
import           Data.Flat.Types
import qualified Data.Foldable                 as F


import           Data.MonoTraversable
import           Data.Sequences as S
import           qualified Data.Sequences as S

import qualified Data.Text                     as T
import           Prelude                 hiding ( mempty )
import qualified Data.Complex as B
import qualified Data.Ratio as B

-- containers
import qualified  Data.Tree as C
import qualified Data.Map                      as M
import qualified Data.IntMap                   as M

import qualified Data.Sequence                 as S
import Data.List

#if MIN_VERSION_base(4,9,0)
import qualified Data.List.NonEmpty as B
#else
-- import           GHC.Generics
deriving instance Generic (B.Complex a)
deriving instance Generic (C.Tree a)
#endif

-- #ifdef ETA_VERSION    
-- import Data.Function(trampoline)
-- import GHC.IO(trampolineIO)
-- #else
-- trampoline = id
-- trampolineIO = id
-- #endif

-- Instances for the base library

-- "7 elements tuples ought to be enough for anybody" (Bill Gates - apocryphal)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d) => Flat (a,b,c,d)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e) => Flat (a,b,c,d,e)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f) => Flat (a,b,c,d,e,f)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f, Flat g) => Flat (a,b,c,d,e,f,g)

instance Flat () where
  encode _ = mempty
  size _ = (+) 0
  decode = pure ()

instance Flat Bool where
  encode = eBool
  size = sBool
  decode = dBool

instance Flat Char where
    size = sChar
    encode = eChar
    decode = dChar
  
instance Flat a => Flat (B.Complex a)

instance (Flat a,Flat b) => Flat (Either a b)

instance Flat Int8 where
  encode = eInt8
  decode = dInt8
  size = sInt8

instance Flat Int16 where
  size = sInt16
  encode = eInt16
  decode = dInt16

instance Flat Int32 where
  size = sInt32
  encode = eInt32
  decode = dInt32

instance Flat Int64 where
  size = sInt64
  encode = eInt64
  decode = dInt64

instance Flat Int where
  size = sInt
  encode = eInt
  decode = dInt

-- Generic list instance (stack overflows with ETA, see https://github.com/typelead/eta/issues/901)
-- instance {-# OVERLAPPABLE #-} Flat a => Flat [a]

instance {-# OVERLAPPABLE #-} Flat a => Flat [a] -- where
  --size [] n = n+1
  --size (h:t) n = trampoline size t (trampoline size h (n+1)) 
  -- size = sizeListWith size -- foldl' (\n e -> ) n
  -- encode = error "BAD"
  -- encode = trampoline . encodeListWith encode
  -- decode = decodeListWith decode

sizeListWith siz l n = foldl' (\n e -> 1 + n + siz e 0) n l

-- For better encoding/decoding performance, it is useful to declare instances of concrete list types.
-- As this one for example:
instance {-# OVERLAPPING #-} Flat [Char]

#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPABLE #-} Flat a => Flat (B.NonEmpty a)
#else

#endif  

instance Flat a => Flat (Maybe a)

instance (Integral a,Flat a) => Flat (B.Ratio a) where
  size a = size (B.numerator a, B.denominator a)
  encode a = encode (B.numerator a,B.denominator a)
  decode = uncurry (B.%) <$> decode

instance Flat Word8 where
  encode = eWord8
  decode = dWord8
  size = sWord8

instance Flat Word16 where
  encode = eWord16
  decode = dWord16
  size = sWord16

instance Flat Word32 where
  encode = eWord32
  decode = dWord32
  size = sWord32

instance Flat Word64 where
  encode = eWord64
  decode = dWord64
  size = sWord64

instance Flat Word where
  size = sWord
  encode = eWord
  decode = dWord

instance Flat Natural where
  size = sNatural
  encode = eNatural
  decode = dNatural

instance Flat Integer where
    size = sInteger
    encode = eInteger
    decode = dInteger
  
instance Flat Float where
  size = sFloat
  encode = eFloat
  decode = dFloat

instance Flat Double where
  size = sDouble
  encode = eDouble
  decode = dDouble

-- Instances for the text library
instance Flat T.Text where
  size = sUTF8Max
  encode = eUTF8
  decode = dUTF8

instance Flat UTF8Text where
 size (UTF8Text t)= sUTF8Max t
 encode (UTF8Text t) = eUTF8 t
 decode = UTF8Text <$> dUTF8

instance Flat UTF16Text where
 size (UTF16Text t)= sUTF16 t
 encode (UTF16Text t) = eUTF16 t
 decode = UTF16Text <$> dUTF16

-- Instances for the bytestring library
instance Flat B.ByteString where
  encode = eBytes
  size = sBytes
  decode = dByteString

instance Flat L.ByteString where
 encode = eLazyBytes
 size = sLazyBytes
 decode = dLazyByteString

instance Flat SBS.ShortByteString where
 encode = eShortBytes
 size = sShortBytes
 decode = dShortByteString

-- TODO Instances for the array library

-- Instances for the dlist? library
-- Constructors not in scope
-- deriving instance Generic (DL.DList a)
-- instance Flat a => Flat (DL.DList a)

-- TODO Instances for the vector library

-- Instances for the mono-traversable library 
{-|
Maps are defined as a list of (Key,Value) tuples:

Map = List (Key,Value)

List a = Nil | Cons a (List a)
-}
-- instance {-# OVERLAPPABLE #-} (Flat (ContainerKey r), Flat (MapValue r), IsMap r) => Flat r where
--   size = sizeMap
--   encode = encodeMap
--   decode = decodeMap


{-| Calculate the size of a map by adding:
* the sizes of all the entries (Key,Value) 
* plus 1 bit per entry (corresponding to the List's Cons)
* plus 1 bit for the final Nil.
-}
sizeMap :: (Flat (ContainerKey r), Flat (MapValue r), IsMap r) => Size r
sizeMap m acc =
  F.foldl' (\acc' (k, v) -> size k (size v (acc' + 1))) (acc + 1)
    . mapToList
    $ m
{-# INLINE sizeMap #-}

{-# INLINE encodeMap #-}
-- |Encode an instance of IsMap, as a list of (Key,Value) tuples
encodeMap
  :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map)
  => map
  -> Encoding
encodeMap = encodeListWith (\(k, v) -> encode k <> encode v) . mapToList
-- encodeMap = go . mapToList
--   where
--     go []     = eFalse
--     go ((!x,!y):xs) = eTrue <> encode x <> encode y <> go xs

{-# INLINE decodeMap #-}
-- |Decode an instance of IsMap, as a list of (Key,Value) tuples
decodeMap
  :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map) => Get map
decodeMap = mapFromList <$> decodeListWith ((,) <$> decode <*> decode)

{-| 
Sequences are defined as Arrays:

Array v = A0
        | A1 v (Array v)
        | A2 v v (Array v)
        ... 
        | A255 ... (Array v)

In practice, this means that the sequence is encoded as a sequence of blocks of up to 255 elements, with every block preceded by the count of the elements in the block and a final 0-length block.

For example, the sequence of Word8 values 11, 22 and 33 is encoded as the byte sequence:
[3,11,22,33,0]
-}
newtype AsList a = AsList {unList::a}
-- instance (IsSequence r, Flat (Element r), IsSequence r) => Flat (AsList r) where
--     size (AsList a) = size (toList a) 
--     encode (AsList a) = encode (toList a)
--     decode = AsList . fromList <$> decode

newtype AsArray a = AsArray {unArray::a}

instance (IsSequence r, Flat (Element r)) => Flat (AsArray r) where
  size (AsArray a) = sizeSequence a
  encode (AsArray a) = encodeSequence a
  decode = AsArray <$> decodeSequence

instance {-# OVERLAPPABLE #-} (Flat (Element r), IsSequence r) => Flat r where
  size = sizeSequence
  encode = encodeSequence
  decode = decodeSequence

-- |Calculate size of an instance of IsSequence as the sum:
-- * of the size of all the elements 
-- * plus the size of the array constructors (1 byte every 255 elements plus one final byte)
sizeSequence
  :: (IsSequence mono, Flat (Element mono)) => mono -> NumBits -> NumBits
sizeSequence s acc = ofoldl' (flip size) acc s + arrayBits (olength s)
{-# INLINE sizeSequence #-}

-- |Encode an instance of IsSequence, as an array
encodeSequence :: (Flat (Element mono), IsSequence mono) => mono -> Encoding
encodeSequence = encodeArrayWith encode . otoList
{-# INLINE encodeSequence #-}

-- |Decode an instance of IsSequence, as an array
decodeSequence :: (Flat (Element b), IsSequence b) => Get b
decodeSequence = fromList <$> decodeArrayWith decode
{-# INLINE decodeSequence #-}

-- Instances for the containers library
instance Flat a => Flat (C.Tree a)
instance Flat a => Flat (C.Forest a)

instance (Flat a, Flat b,Ord a) => Flat (M.Map a b) where
  size = sizeMap
  encode = encodeMap
  decode = decodeMap

instance Flat a => Flat (M.IntMap a) where
     size = sizeMap
     encode = encodeMap
     decode = decodeMap

-- instance Flat a => Flat (S.Seq a) where
--  size = sizeSequence
--  encode = encodeSequence
--  decode = decodeSequence

