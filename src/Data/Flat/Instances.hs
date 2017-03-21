{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
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
import qualified Data.Sequences        as O
import qualified Data.Text             as T
import           Data.Word
import           Numeric.Natural
import           Prelude               hiding (mempty)

-- import qualified Data.Vector            as V
-- import qualified Data.Vector.Unboxed            as VU
-- import qualified Data.Vector.Storable as VS

---------- Flat Instances
instance Flat () where
  encode _ = mempty
  decode = pure ()

instance Flat Bool where
  encode = eBool
  size = sBool
  decode = dBool

instance Flat a => Flat (Maybe a)

instance (Flat a,Flat b) => Flat (Either a b)

-- data Map a b = Map [(a,b)]
instance (Flat a, Flat b,Ord a) => Flat (M.Map a b) where
  encode = encode . M.toList
  -- size = cmapSize M.toList
  size = size . M.toList
  decode = M.fromList <$> decode

-- == Array Word8 (prob: encoder might fail if used on their own)
-- or BLOB NoEncoding
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
  size s acc = F.foldl' (flip size) acc s + arrayBits (S.length s)
  encode = eArray encode . F.toList
  decode = S.fromList <$> dArray decode

{-# INLINE sSequence #-}
sSequence
  :: (MonoFoldable mono, Flat (Element mono)) =>
     mono -> NumBits -> NumBits
sSequence s acc = ofoldl' (flip size) acc s + arrayBits (olength s)

instance {-# OVERLAPPABLE #-} (MonoFoldable mono, O.IsSequence mono, Flat (Element mono)) => Flat mono where
  size = sSequence

  {-# INLINE encode #-}
  encode = eArray encode . otoList

  {-# INLINE decode #-}
  decode = O.pack <$> dArray decode -- dList -- try with O.replicateM

instance {-# OVERLAPPABLE #-} Flat a => Flat [a]

instance {-# OVERLAPPING #-} Flat [Char]

-- BLOB UTF8Encoding
instance Flat T.Text where
   size = sUTF16
   encode = eUTF16
   decode = dUTF16

---------- Words and Ints

-- x = map (\v -> showEncoding $ encode (v::Word32)) [3,255,258]
{-
-- Little Endian encoding
| Coding                             | Min Len | Max Len   |
| data Unsigned = NonEmptyList Word7 | 8       | 10*8=80   | ascii equivalent,byte align
| data Unsigned = NonEmptyList Word8 | 9       | 8*9=72    |
| data Unsigned = List Word7         | 1       | 10*8+1=81 | ascii equivalent

-}
-- Encoded as: data Word8 = U0 | U1 .. | U255
instance Flat Word8 where
  encode = eWord8
  decode = dWord8
  size = sWord8

{- Word16 to Word64 are encoded as:
-- data VarWord = VarWord (NonEmptyList Word7)
-- data NonEmptyList a = Elem a | Cons a (NonEmptyList a)
-- data Word7 = U0 .. U127
-- VarWord is a sequence of Word7, where every byte except the last one has the most significant bit (msb) set.

as in google protocol buffer VarInt

Example:
3450 :: Word16/32/64.. = 0000110101111010 = 11010(26) 1111010(122) coded as:
Word16 (Cons V122 (Elem V26))
so Least Significant Byte first.
-}
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

-- Encoded as data Int8 = Z | N1 |P1| N2 |P2 | N3 .. |P127 | N128
instance Flat Int8 where
  encode = eInt8
  decode = dInt8
  size = sInt8

-- Ints and Integer are encoded as
-- Encoded as ZigZag Word16
-- ZigZag indicates ZigZag encoding
-- where data ZigZag a = ZigZag a
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

--------------- Floats
instance Flat Float where
  size = sFloat
  encode = eFloat
  decode = dFloat

instance Flat Double where
  size = sDouble
  encode = eDouble
  decode = dDouble

----------------- Characters
-- data Char = Char Word32
instance Flat Char where
  size = sChar
  encode = eChar
  decode = dChar

{-
-- data Unicode = Unicode Word32
instance Flat Char where
  encode c = encode (fromIntegral . ord $ c :: Word32)

  decode = do
    w :: Word32 <- decode
    if w > 0x10FFFF
      then error $ "Not a valid Unicode code point: " ++ show w
      else return . chr .fromIntegral $ w
-}


---------- Tuples

instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d) => Flat (a,b,c,d)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e) => Flat (a,b,c,d,e)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f) => Flat (a,b,c,d,e,f)
instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f, Flat g) => Flat (a,b,c,d,e,f,g)
