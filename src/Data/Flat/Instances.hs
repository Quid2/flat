{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |Flat Instances for common, primitive or 'opaque' data types for which instances cannot be automatically derived
module Data.Flat.Instances () where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Char
import           Data.Flat.Class
import           Data.Flat.Decoder
import           Data.Flat.Encoder
import qualified Data.Foldable         as F
import           Data.Int
import qualified Data.Map              as M
import           Data.MonoTraversable
import qualified Data.Sequence         as S
-- import qualified Data.Sequences        as O
import qualified Data.Text             as T
import           Data.Word
import           Numeric.Natural
import           Prelude               hiding (mempty)

-- import qualified Data.Vector            as V
-- import qualified Data.Vector.Unboxed            as VU
-- import qualified Data.Vector.Storable as VS

---------- Common Types

instance Flat () where
  encode _ = mempty
  decode = pure ()

instance Flat Bool where
  encode = eBool
  size = sBool
  decode = dBool

instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d) => Flat (a,b,c,d)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e) => Flat (a,b,c,d,e)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f) => Flat (a,b,c,d,e,f)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f, Flat g) => Flat (a,b,c,d,e,f,g)

-- Primitive types
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

instance Flat a => Flat (Maybe a)

instance (Flat a,Flat b) => Flat (Either a b)

-- Do not provide this to 'force' users to declare instances of concrete list types
-- instance Flat a => Flat [a]
instance Flat [Char]
--instance {-# OVERLAPPABLE #-} Flat a => Flat [a]
--instance {-# OVERLAPPING #-} Flat [Char]

-- BLOB UTF8Encoding
instance Flat T.Text where
   size = sUTF16
   encode = eUTF16
   decode = dUTF16

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

-- Abstract/Opaque types

instance (Flat a, Flat b,Ord a) => Flat (M.Map a b) where
  --size = sizeList M.toList
  size = sizeList_
  --encode = encodeList M.toList
  -- encode = encodeList2_
  encode = encodeList_ .  M.toList
  decode = M.fromList <$> decodeList_

-- data Array a = Array0 | Array1 a ...
--data Array a = Array [a] deriving (Eq, Ord, Show, NFData, Generic)

-- instance {-# OVERLAPPABLE #-} Flat a => Flat (Array a) where
--     encode (Array l) = encodeArray l
--     -- size = arraySize
--     -- size = arraySize
--     decode = Array <$> dArray

-- arraySize (Array vs) = 8*(numBlks 255 (length vs) + 1) + sum (map size vs)
-- INEFFICIENT
-- arraySize (Array vs) = 8*(numBlks 255 (length vs) + 1) + sum (map (size 0) vs)

-- instance {-# OVERLAPPING #-} Flat (Array Word8) where
--     encode (Array l) = encode $ B.pack l
--     decode = Array . B.unpack <$> decode

instance Flat a => Flat (S.Seq a) where
  size = sizeArray F.foldl' S.length
  encode = encodeArray F.toList
  decode = decodeArray S.fromList

{-# INLINE sizeList #-}
sizeList
  :: (Foldable t, Flat a) => (t1 -> t a) -> t1 -> NumBits -> NumBits
sizeList toList l = sizeList_ (toList l)
-- sizeList = sizeList_

{-# INLINE sizeList_ #-}
sizeList_ :: (Foldable t, Flat a) => t a -> NumBits -> NumBits
sizeList_ l acc = F.foldl' (\acc n -> size n (acc + 1)) (acc+1) l

{-# INLINE encodeList #-}
encodeList toList = encodeList_ . toList

{-# INLINE encodeList_ #-}
encodeList_ :: Flat a => [a] -> Encoding
encodeList_ [] = eFalse
encodeList_ (x:xs) = eTrue <> encode x <> encodeList_ xs

encodeList2_ l = F.foldl' (\e n -> e <> eTrue <> encode n) mempty l <> eFalse 

{-# INLINE decodeList #-}
decodeList :: Flat a => ([a] -> b) -> Get b
decodeList fromList = fromList <$> decodeList_

{-# INLINE decodeList_ #-}
decodeList_ :: Flat a => Get [a]
decodeList_ = do
  b <- dBool
  if b
    then (:) <$> decode <*> decodeList_
    else return []

{-# INLINE sizeArray #-}
sizeArray foldl length s acc = foldl (flip size) acc s + arrayBits (length s)

{-# INLINE encodeArray #-}
encodeArray toList = eArray encode . toList

{-# INLINE decodeArray #-}
decodeArray fromList = fromList <$> dArray decode

{-# INLINE sSequence #-}
sSequence
  :: (MonoFoldable mono, Flat (Element mono)) =>
     mono -> NumBits -> NumBits
sSequence s acc = ofoldl' (flip size) acc s + arrayBits (olength s)

-- instance {-# OVERLAPPABLE #-} (MonoFoldable mono, O.IsSequence mono, Flat (Element mono)) => Flat mono where
--   size = sSequence

--   {-# INLINE encode #-}
--   encode = eArray encode . otoList

--   {-# INLINE decode #-}
--   decode = O.pack <$> dArray decode -- dList -- try with O.replicateM
