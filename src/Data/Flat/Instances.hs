-- {-# LANGUAGE BangPatterns #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE UndecidableInstances    #-}
-- {-# LANGUAGE IncoherentInstances    #-}

-- |Flat Instances for common, primitive and abstract data types for which instances cannot be automatically derived
module Data.Flat.Instances (
    sizeMap,
    encodeMap,
    decodeMap,
    sizeSet,
    encodeSet,
    decodeSet,
    sizeSequence,
    encodeSequence,
    decodeSequence,
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Char
import           Data.Containers       (ContainerKey, IsMap, IsSet, MapValue,
                                        mapFromList, mapToList, setFromList,
                                        setToList)
import           Data.Flat.Class
import           Data.Flat.Decoder
import           Data.Flat.Encoder
--import           Data.Flat.Size        (arrayBits)
import           Data.Flat.Types
import qualified Data.Foldable         as F
import           Data.IntMap           (IntMap)
import           Data.IntSet           (IntSet)
import           Data.Map              (Map)
import           Data.MonoTraversable
import           Data.Sequence         (Seq)
import           Data.Sequences
import           Data.Set              (Set)
import qualified Data.Text             as T
import           Prelude               hiding (mempty)

-- Flat instances for common types
instance Flat () where
  encode _ = mempty
  decode = pure ()

instance Flat Bool where
  encode = eBool
  size = sBool
  decode = dBool

instance Flat a => Flat (Maybe a)

instance (Flat a,Flat b) => Flat (Either a b)

instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d) => Flat (a,b,c,d)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e) => Flat (a,b,c,d,e)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f) => Flat (a,b,c,d,e,f)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f, Flat g) => Flat (a,b,c,d,e,f,g)

-- Do not provide this to 'force' users to declare instances of concrete list types
instance {-# OVERLAPPABLE #-} Flat a => Flat [a]
instance {-# OVERLAPPING #-} Flat [Char]

-- instance Flat [Char]

-- Flat instances for primitive/abstract types
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

instance Flat Integer where
  size = sInteger
  encode = eInteger
  decode = dInteger

instance Flat Natural where
  size = sNatural
  encode = eNatural
  decode = dNatural

instance Flat Float where
  size = sFloat
  encode = eFloat
  decode = dFloat

instance Flat Double where
  size = sDouble
  encode = eDouble
  decode = dDouble

instance Flat Char where
  size = sChar
  encode = eChar
  decode = dChar

instance Flat a => Flat (IntMap a) where
  size = sizeMap
  encode = encodeMap
  decode = decodeMap

instance Flat IntSet where
  size = sizeSet
  encode = encodeSet
  decode = decodeSet

instance (Ord a, Flat a, Flat b) => Flat (Map a b) where
  size = sizeMap
  encode = encodeMap
  decode = decodeMap

instance Flat a => Flat (Seq a) where
  size = sizeSequence
  encode = encodeSequence
  decode = decodeSequence

instance (Ord a, Flat a) => Flat (Set a) where
  size = sizeSet
  encode = encodeSet
  decode = decodeSet

-- |Calculate size of an instance of IsMap
{-# INLINE sizeMap #-}
sizeMap :: (Flat (ContainerKey r), Flat (MapValue r), IsMap r) => Size r
sizeMap m acc = F.foldl' (\acc' (k,v) -> size k (size v (acc' + 1))) (acc+1) . mapToList $ m

{-# INLINE encodeMap #-}
-- |Encode an instance of IsMap, as a list
encodeMap :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map) => map -> Encoding
encodeMap = encodeListWith (\(k,v) -> encode k <> encode v) . mapToList
-- encodeMap = go . mapToList
--   where
--     go []     = eFalse
--     go ((!x,!y):xs) = eTrue <> encode x <> encode y <> go xs

{-# INLINE decodeMap #-}
-- |Decode an instance of IsMap, as a list
decodeMap :: (Flat (ContainerKey map), Flat (MapValue map), IsMap map) => Get map
decodeMap = mapFromList <$> decodeListWith ((,) <$> decode <*> decode)

{-# INLINE sizeSet #-}
-- |Calculate size of an instance of IsSet
sizeSet :: (Flat (ContainerKey r), IsSet r) => Size r
sizeSet m acc = F.foldl' (\acc' k -> size k (acc' + 1)) (acc + 1) . setToList $ m

{-# INLINE encodeSet #-}
-- |Encode an instance of IsSet, as a list
encodeSet :: (Flat (ContainerKey set), IsSet set) => set -> Encoding
encodeSet = encodeListWith encode . setToList

{-# INLINE decodeSet #-}
-- |Decode an instance of IsSet, as a list
decodeSet :: (Flat (ContainerKey set), IsSet set) => Get set
decodeSet = setFromList <$> decodeListWith decode

{-# INLINE sizeSequence #-}
-- |Calculate size of an instance of IsSequence
sizeSequence :: (IsSequence mono, Flat (Element mono)) => mono -> NumBits -> NumBits
sizeSequence s acc = ofoldl' (flip size) acc s + arrayBits (olength s)

{-# INLINE encodeSequence #-}
-- |Encode an instance of IsSequence, as an array
encodeSequence :: (Flat (Element mono), IsSequence mono) => mono -> Encoding
encodeSequence = encodeArrayWith encode . otoList

{-# INLINE decodeSequence #-}
-- |Decode an instance of IsSequence, as an array
decodeSequence :: (Flat (Element b), IsSequence b) => Get b
decodeSequence = fromList <$> decodeArrayWith decode
