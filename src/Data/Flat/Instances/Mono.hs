{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances ,NoMonomorphismRestriction #-}
module Data.Flat.Instances.Mono
  ( sizeSequence
  , encodeSequence
  , decodeSequence
  , sizeList
  , encodeList
  , decodeList
  , sizeSet
  , encodeSet
  , decodeSet
  , sizeMap
  , encodeMap
  , decodeMap
  , AsArray(..)
  , AsList(..)
  , AsSet(..)
  , AsMap(..)
  )
where

import           Data.MonoTraversable           ( Element
                                                , ofoldl'
                                                , otoList
                                                --, olength
                                                , MonoFoldable
                                                )
import           Data.Sequences                 ( IsSequence )
import qualified Data.Sequences                as S
import           Data.Containers
-- ( ContainerKey
--                                                 , IsMap
--                                                 , MapValue
--                                                 , mapFromList
--                                                 , mapToList
--                                                 , IsSet
--                                                 , setToList
--                                                 , setFromList
--                                                 )
import           Data.Flat.Instances.Util
import qualified Data.Foldable                 as F

-- $setup
-- >>> import Data.Flat.Instances.Base()
-- >>> import Data.Flat.Instances.Test
-- >>> import Data.Word    
-- >>> import qualified Data.Set
-- >>> import qualified Data.Map

{-|
Sequences are defined as Arrays:

Array v = A0
        | A1 v (Array v)
        | A2 v v (Array v)
        ...
        | A255 ... (Array v)

In practice, this means that the sequence is encoded as a sequence of blocks of up to 255 elements, with every block preceded by the count of the elements in the block and a final 0-length block.

Lists are defined as:

List a ≡  Nil
        | Cons a (List a)

The AsList/AsArray wrappers can be used to serialise sequences as Lists or Arrays

>>> tst $ AsArray ([]::[()])
(True,8,[0])

>>> tst $ AsArray [11::Word8,22,33]
(True,40,[3,11,22,33,0])

>>> tst $ AsList ([]::[()])
(True,1,[0])

>>> tst (AsList [11::Word8,22,33])
(True,28,[133,197,164,32])

>>> tst (AsSet (Data.Set.fromList [11::Word8,22,33]))
(True,28,[133,197,164,32])

-}
newtype AsArray a =
  AsArray
    { unArray :: a
    } deriving (Show,Eq,Ord)

instance (IsSequence r, Flat (Element r)) => Flat (AsArray r) where
  size (AsArray a) = sizeSequence a
  encode (AsArray a) = encodeSequence a
  decode = AsArray <$> decodeSequence

-- |Calculate size of an instance of IsSequence as the sum:
-- * of the size of all the elements
-- * plus the size of the array constructors (1 byte every 255 elements plus one final byte)
sizeSequence
  :: (IsSequence mono, Flat (Element mono)) => mono -> NumBits -> NumBits
sizeSequence s acc =
  let (sz, len) =
          ofoldl' (\(acc, l) e -> (size e acc, l + 1)) (acc, 0 :: NumBits) s
  in  sz + arrayBits len
{-# INLINE sizeSequence #-}

-- TODO: check which one is faster
-- sizeSequence s acc = ofoldl' (flip size) acc s + arrayBits (olength s)

-- |Encode an instance of IsSequence, as an array
encodeSequence :: (Flat (Element mono), MonoFoldable mono) => mono -> Encoding
encodeSequence = encodeArray . otoList
{-# INLINE encodeSequence #-}

-- |Decode an instance of IsSequence, as an array
decodeSequence :: (Flat (Element b), IsSequence b) => Get b
decodeSequence = S.fromList <$> decodeArrayWith decode
{-# INLINE decodeSequence #-}

newtype AsList a =
  AsList
    { unList :: a
    } deriving (Show,Eq,Ord)

instance (IsSequence l, Flat (Element l)) => Flat (AsList l) where
  -- size   = sizeList . S.unpack . unList
  -- encode = encodeList . S.unpack . unList
  -- decode = AsList . S.fromList <$> decodeListotoList

  size   = sizeList . unList
  encode = encodeList . unList
  decode = AsList <$> decodeList

{-# INLINE sizeList #-}
sizeList :: (MonoFoldable mono, Flat (Element mono)) => mono -> NumBits -> NumBits
sizeList l sz = ofoldl' (\s e -> size e (s + 1)) (sz + 1) l

{-# INLINE encodeList #-}
encodeList :: (Flat (Element mono), MonoFoldable mono) => mono -> Encoding
encodeList = encodeListWith encode  . otoList

{-# INLINE decodeList #-}
decodeList :: (IsSequence b, Flat (Element b)) => Get b
decodeList = S.fromList <$> decodeListWith decode

newtype AsSet a =
  AsSet
    { unSet :: a
    } deriving (Show,Eq,Ord)

instance (IsSet set, Flat (Element set)) => Flat (AsSet set) where
  size   = sizeSet . unSet
  encode = encodeSet . unSet
  decode = AsSet <$> decodeSet

sizeSet :: (IsSet set, Flat (Element set)) => Size set
sizeSet l acc = ofoldl' (\acc e -> size e (acc + 1)) (acc + 1) $ l
{-# INLINE sizeSet #-}

encodeSet :: (IsSet set, Flat (Element set)) => set -> Encoding
encodeSet = encodeList . setToList
{-# INLINE encodeSet #-}

decodeSet :: (IsSet set, Flat (Element set)) => Get set
decodeSet = setFromList <$> decodeList
{-# INLINE decodeSet #-}

{-|
Maps are saved as lists of (key,value) tuples.

>>> tst (AsMap (Data.Map.fromList ([]::[(Word8,())])))
(True,1,[0])

>>> tst (AsMap (Data.Map.fromList [(3::Word,9::Word)]))
(True,18,[129,132,128])
-}
newtype AsMap a =
  AsMap
    { unMap :: a
    } deriving (Show,Eq,Ord)

instance (IsMap map, Flat (ContainerKey map), Flat (MapValue map)) => Flat (AsMap map) where
  size   = sizeMap . unMap
  encode = encodeMap . unMap
  decode = AsMap <$> decodeMap

{-# INLINE sizeMap #-}
sizeMap :: (Flat (ContainerKey r), Flat (MapValue r), IsMap r) => Size r
sizeMap m acc = F.foldl' (\acc' (k, v) -> size k (size v (acc' + 1))) (acc + 1) . mapToList $ m
-- sizeMap l sz = ofoldl' (\s (k, v) -> size k (size v (s + 1))) (sz + 1) l

{-# INLINE encodeMap #-}
-- |Encode an instance of IsMap, as a list of (Key,Value) tuples
encodeMap
  :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map)
  => map
  -> Encoding
encodeMap = encodeListWith (\(k, v) -> encode k <> encode v) . mapToList

{-# INLINE decodeMap #-}
-- |Decode an instance of IsMap, as a list of (Key,Value) tuples
decodeMap
  :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map) => Get map
decodeMap = mapFromList <$> decodeListWith ((,) <$> decode <*> decode)

