{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction ,ScopedTypeVariables #-}
module Data.Flat.Instances (
  Array(..)
  ) where

import           Control.DeepSeq
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as SBS
import           Data.Char
import           Data.Flat.Class
import           Data.Flat.Encoder
import           Data.Flat.Decoder
import           Data.Int
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Word
import           Data.ZigZag
import           Prelude hiding (mempty)
import           Data.MonoTraversable
import qualified Data.Sequences as O
import           Numeric.Natural

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
data Array a = Array [a]
  deriving (Eq, Ord, Show, NFData, Generic)

-- instance {-# OVERLAPPABLE #-} Flat a => Flat (Array a) where
--     encode (Array l) = encodeArray l
--     size = error "unimplemented"
--     -- size = arraySize
--     -- size = arraySize
--     decode = Array <$> dArray

-- arraySize (Array vs) = 8*(numBlks 255 (length vs) + 1) + sum (map size vs)
-- INEFFICIENT
-- arraySize (Array vs) = 8*(numBlks 255 (length vs) + 1) + sum (map (size 0) vs)

-- instance {-# OVERLAPPING #-} Flat (Array Word8) where
--     encode (Array l) = encode $ B.pack l
--     decode = Array . B.unpack <$> decode

{-# INLINE dList #-}
dList :: Flat a => Get [a]
dList = do
    tag <- dBool
    if tag
      then (:) <$> decode <*> dList
      else return []

-- 1 2 3 -> 3 : 2 : 1
{-# INLINE dListReverse #-}
dListReverse :: Flat a => Get [a]
dListReverse = go []
    where go !acc = do
            tag <- dBool
            if tag
              then do
                x <- decode
                go (x:acc)
              else return $! reverse acc

-- #define LIST_BYTE
#define ENCLIST_DIV

-- Different implementations of encoding for Array (none very good)
-- #ifdef ENCLIST_GO
-- encodeArray :: Flat a => [a] -> Encoding
-- encodeArray l = go mempty l (length l)
--     where
--       go e !l 0 = e <> eWord8 0
--       go e !l n = let c = min 255 n
--                       n' = n-c
--                       (e',!l') = goElems (e <> (eWord8 $ fromIntegral c)) l c
--                  in go e' l' n'
--       goElems e !l      0 = (e,l)
--       goElems e (!x:xs) n = goElems (e <> encode x) xs (n-1)

-- #elif defined (ENCLIST_DIV)
-- encodeArray l = let (d,m) = length l `divMod` 255
--                     ns = cons d $ if m==0 then [0] else [m,0]
--                     cons 0 t = t
--                     cons n t = cons (n-1) (255:t)
--                 in gos ns l
--   where
--     gos [] [] = mempty
--     gos (n:ns) l = eWord8 (fromIntegral n) <> go ns n l
--     go ns 0 l = gos ns l
--     go ns n (h:t) = encode h <> go ns (n-1) t

-- #elif defined(ENCLIST_FOLDL2)
-- encodeArray :: Flat a => [a] -> Encoding
-- encodeArray l = let (e,0,_) = encList l in e <> eWord8 0

-- encList l  = foldl' (\(!r,!l,!s) x ->
--                       if s==0
--                       then (r <> eWord8 (fromIntegral (min l 255)) <> encode x,l-1,254)
--                       else (r <> encode x,l-1,s-1)
--                     )
--              (mempty,length l,0) l
-- #endif

-- dArray :: Flat a => Get [a]

--pokeSequence :: (IsSequence t, Flat (Element t)) => t -> Poke ()
--sizeSequence =  ofoldl' (\acc x -> stepSize (encode x) 1 + acc + f x) (sizeOf (undefined :: Int)) t

-- 8-15x
-- eList l = let vs = map encode l in Step (foldl' (\n st -> n+1+stepSize st) 1 vs) (\s -> (foldM (\s st -> eTrueF s >>= stepF st) s vs) >>= eFalseF)

-- eSequence :: (MonoFoldable mono, Flat (Element mono)) => mono -> Encoding
--eSequence t = let vs = omap encode t
--              in Step (sum $ omap stepSize vs) (\s -> ofoldM (\s k->k s) s vs)

-- 5.2x
-- eSequence = ofoldr (\x r -> eTrueS <| encode x <> r) eFalse

-- 3x lazy if we have simple elements, 9x if not? as the eListElem optimisation doesn't work
-- 9-11x for strict
-- eSequence t = ofoldMap (eListElem . encode) t <> eFalse

-- -20%
-- eSequence t = ofoldMap encode t <> eFalse
-- eSequence t = error "PUT THIS BACK" -- ofoldMap encode t <> eFalse

-- 5-9x
-- eSequence t = ofoldMap (\x -> eTrue <> encode x) t <> eFalse
-- {-# INLINE eSequence #-}


{-# INLINE sSequence #-}

-- sSequence :: forall s. (O.IsSequence s, Flat (Element s)) => Size s
-- sSequence = VarSize $ \s ->
--   let l = olength s
--   in case size :: Size (Element s) of
--        ConstSize n -> arrayBits l + n * l
--        VarSize f -> arrayBits l + ofoldl' (\acc x -> acc + f x) 0 s

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

instance {-# OVERLAPPABLE #-} Flat a => Flat [a] -- where
--    decode = dList

-- #define LIST_TAG

-- instance {-# OVERLAPPABLE #-} Flat a => Flat [a] where
-- #ifdef LIST_BIT
--     -- encode = foldr (\x r -> eTrue <> encode x <> r) eFalse
--     encode = eSequence
-- #elif defined(LIST_TAG)
--     encode = eBitList . map encode
-- #ifdef ARRDEC_DIRECT
--     decode = dList
-- #elif defined(ARRDEC_REVERSE)
--     decode = dListReverse
-- #endif
-- #elif defined(LIST_BYTE)
--     encode = encodeArray
--     decode = dArray
-- #endif

-- instance {-# OVERLAPPING #-} Flat [Char] where
--     encode = encode . T.pack
--     decode = T.unpack <$> decode

-- instance {-# OVERLAPPING #-} Flat [Char] where
--     -- encode s = eFiller <> eArrayChar s
--   encode = eArrayChar
--   decode = T.unpack <$> decode

instance {-# OVERLAPPING #-} Flat [Char] -- where
  -- encode = eString
  -- size = sString

-- BLOB UTF8Encoding
instance Flat T.Text where
  -- 100 times slower
  -- encode l = (mconcat . map (\t -> T.foldl' (\r x -> r <> encode x) (eWord8 . fromIntegral . T.length$ t) t) . T.chunksOf 255 $ l) <> eWord8 0
    -- -- 200 times slower
    -- encode = encode . T.unpack
    -- decode = T.pack <$> dArray

  -- 4 times slower
   --encode = encode . blob UTF8Encoding . L.fromStrict . T.encodeUtf8
   --decode = T.decodeUtf8 . L.toStrict . (unblob :: BLOB UTF8Encoding -> L.ByteString) <$> decode

   size = sUTF16
   encode = eUTF16
   -- encode = eText
   decode = dUTF16

---------- Words and Ints

-- x = map (\v -> showEncoding $ encode (v::Word32)) [3,255,258]
{-
-- Little Endian encoding
| Coding                             | Min Len | Max Len   |
| data Unsigned = NonEmptyList Word7 | 8       | 10*8=80   | ascii equivalent,byte align
| data Unsigned = NonEmptyList Word8 | 9       | 8*9=72    |
| data Unsigned = List Word7         | 1       | 10*8+1=81 | ascii equivalent

data Integer = Integer (ZigZag VarWord)

data Int16 = Int16 (ZigZag Word16)

data Int8 = Int8 (ZigZag Word8)

data ZigZagEncoding a = ZigZagEncoding a

data Word16 = Word16 VarWord

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
  encode = encode . zzEncode8
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

-- instance Flat Word7 where
--     encode = eBits 7 . fromIntegral . fromEnum
--     decode = toEnum . fromIntegral <$> dBits 7

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
-- data ASCII = ASCII Word7 deriving (Eq,Show,Generic)

-- instance Flat ASCII

-- -- BUG
-- w :: Word7
-- w =  toEnum 200 :: Word7

-- -- t2 = let i = 240 in (fromIntegral i :: Word7) <= (maxBound :: Word7)

-- g :: (Word7,Word7)
-- g = (minBound,maxBound)

-- t3 = fromASCII . toASCII $ 'à' -- '经'

-- toASCII :: Char -> ASCII
-- toASCII = ASCII . toEnum . ord

-- fromASCII :: ASCII -> Char
-- fromASCII (ASCII w7) = chr . fromIntegral $ w7

-- data Unicode = C0 .. C127 | Unicode Word32

{-
toUnicode c | ord c <=127 = C0
            | otherwise = Unicode . fromIntegral . ord $ c
-}
-- data Char = Char Word32
-- PROB: Step 40 rather than 24
instance Flat Char where
  size = sChar
  encode = eChar
  decode = dChar

  -- encode c | ord c <=127 = eFalse <> eBits 7 (fromIntegral . ord $ c)
  --          | otherwise   = eTrue  <> encode (fromIntegral . ord $ c :: Word32)

  -- decode =  do
  --     tag <- dBool
 --     if tag
  --       then chr . fromIntegral <$> (decode :: Get Word32)
  --       else chr . fromIntegral <$> dBits 7

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

instance {-# OVERLAPPABLE #-} (Flat a, Flat b) => Flat (a,b) where
  --{-# INLINE encode #-}
  -- encode (a,b)           = encode a <+> encode b
  --encode (a,b)           = encoders [encode a,encode b]
  decode                 = (,) <$> decode <*> decode

instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c) => Flat (a,b,c) where
  -- {-# INLINE encode #-}
  -- encode (a,b,c)         = encode a <+> encode b <+> encode c
  -- encode (a,b,c)         =  encoders [encode a,encode b,encode c]
  -- encode (c,d,e) =  chkWriter (encode c) [encode d,encode e] <> chkWriter (encode d) [encode e] <> encode e
  decode                 =  (,,) <$> decode <*> decode <*> decode

instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d) => Flat (a,b,c,d) where
  -- {-# INLINE encode #-}
  --encode (a,b,c,d)       = encode a <+> encode b <+> encode c <+> encode d
  -- encode (a,b,c,d)         =  encoders [encode a,encode b,encode c,encode d]
  -- encode (b,c,d,e) =  chkWriter (encode b) [encode c,encode d,encode e] <> chkWriter (encode c) [encode d,encode e] <> chkWriter (encode d) [encode e] <> encode e
  decode                 = (,,,) <$> decode <*> decode <*> decode <*> decode

instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e) => Flat (a,b,c,d,e) where
  --{-# INLINE encode #-}
  --encode (a,b,c,d,e)     = encode a <+> encode b <+> encode c <+> encode d <+> encode e
  --encode (a,b,c,d,e)         =  encoders [encode a,encode b,encode c,encode d,encode e]
  -- encode (a,b,c,d,e)         = error "WORDS TUPLE"
  -- encode (a,b,c,d,e)         = chkWriter (encode a) [encode b,encode c,encode d,encode e] <> chkWriter (encode b) [encode c,encode d,encode e] <> chkWriter (encode c) [encode d,encode e] <> chkWriter (encode d) [encode e] <> encode e
  decode                 = (,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode

instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f)
        => Flat (a,b,c,d,e,f) where
  --{-# INLINE encode #-}
  -- encode (a,b,c,d,e,f)   = encode (a,(b,c,d,e,f))
  --encode (a,b,c,d,e,f)         =  encoders [encode a,encode b,encode c,encode d,encode e,encode f]
  decode                 = (,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e, Flat f, Flat g)
        => Flat (a,b,c,d,e,f,g) where
  --{-# INLINE encode #-}
  --encode (a,b,c,d,e,f,g) = encode (a,(b,c,d,e,f,g))
  decode                 = (,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

-- instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e,
--           Flat f, Flat g, Flat h)
--         => Flat (a,b,c,d,e,f,g,h) where
--   --{-# INLINE encode #-}
--   --encode (a,b,c,d,e,f,g,h) = encode (a,(b,c,d,e,f,g,h))
--   decode                   = (,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

-- instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e,
--           Flat f, Flat g, Flat h, Flat i)
--         => Flat (a,b,c,d,e,f,g,h,i) where
--   --{-# INLINE encode #-}
--   --encode (a,b,c,d,e,f,g,h,i) = encode (a,(b,c,d,e,f,g,h,i))
--   decode                     = (,,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

-- instance {-# OVERLAPPABLE #-} (Flat a, Flat b, Flat c, Flat d, Flat e,
--           Flat f, Flat g, Flat h, Flat i, Flat j)
--         => Flat (a,b,c,d,e,f,g,h,i,j) where
--   --{-# INLINE encode #-}
--   --encode (a,b,c,d,e,f,g,h,i,j) = encode (a,(b,c,d,e,f,g,h,i,j))
--   decode                       = (,,,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

