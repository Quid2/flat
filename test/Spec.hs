{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NegativeLiterals          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | Tests for the flat module

import           Data.Char
import           Data.DeriveTH
import           Data.Int
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as T

import           Data.Flat
import           Data.List
import           Data.Ord
import           Test.Data
import           Test.Data.Flat

t = main

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties
                           ,unitTests]

properties = testGroup "Properties"
  [  rt "()" (prop_Flat_roundtrip:: RT ())
    ,rt "Bool" (prop_Flat_roundtrip::RT Bool)
    ,rt "(Bool,Word8,Bool)" (prop_Flat_roundtrip:: RT (Bool,Word8,Bool))
    ,rt "Word8" (prop_Flat_Large_roundtrip:: RTL Word8)
    ,rt "Word16" (prop_Flat_Large_roundtrip:: RTL Word16)
    ,rt "Word32" (prop_Flat_Large_roundtrip:: RTL Word32)
    ,rt "Word64" (prop_Flat_Large_roundtrip:: RTL Word64)
    ,rt "Word" (prop_Flat_Large_roundtrip:: RTL Word)
    ,rt "Int8" (prop_Flat_Large_roundtrip:: RTL Int8)
    ,rt "Int16" (prop_Flat_Large_roundtrip:: RTL Int16)
    ,rt "Int32" (prop_Flat_Large_roundtrip:: RTL Int32)
    ,rt "Int64" (prop_Flat_Large_roundtrip:: RTL Int64)
    ,rt "Int" (prop_Flat_Large_roundtrip:: RTL Int)
    ,rt "Integer" (prop_Flat_roundtrip:: RT Integer)
    ,rt "Char" (prop_Flat_roundtrip:: RT Char)
    --,rt "ASCII" (prop_Flat_roundtrip:: RT ASCII)
    ,rt "Unit" (prop_Flat_roundtrip:: RT Unit)
    ,rt "Un" (prop_Flat_roundtrip:: RT Un )
    ,rt "N" (prop_Flat_roundtrip:: RT N )
    ,rt "A" (prop_Flat_roundtrip:: RT A )
    ,rt "B" (prop_Flat_roundtrip:: RT B )
    ,rt "Maybe N" (prop_Flat_roundtrip:: RT (Maybe N))
    ,rt "Either N Bool" (prop_Flat_roundtrip:: RT (Either N Bool))
    ,rt "Either Int Char" (prop_Flat_roundtrip:: RT (Either Int Char))
    --,rt "Tree Bool" (prop_Flat_roundtrip:: RT (Tree Bool))
    -- ,rt "Tree N" (prop_Flat_roundtrip:: RT (Tree N))
    ,rt "List N" (prop_Flat_roundtrip:: RT (List N))
    ,rt "[Int16]" (prop_Flat_roundtrip:: RT [Int16])
    ,rt "String" (prop_Flat_roundtrip:: RT String)
    ,rt "Text" (prop_Flat_roundtrip:: RT T.Text)
    ,rt "ByteString" (prop_Flat_roundtrip:: RT B.ByteString)
    ,rt "Lazy ByteString" (prop_Flat_roundtrip:: RT L.ByteString)
  ]
   where rt n = QC.testProperty (unwords ["round trip",n])

unitTests = testGroup "Serialisation Unit tests" $ concat [
             s () []
            ,s ((),()) []
            ,a () [1]
            ,a True [128+1]
            ,a (True,True) [128+64+1]
            ,a (True,False,True) [128+32+1]
            ,a (True,False,True,True) [128+32+16+1]
            ,a (True,False,True,True,True) [128+32+16+8+1]
            ,a (True,False,True,True,True,True) [128+32+16+8+4+1]
            ,a (True,False,True,True,True,True,True) [128+32+16+8+4+2+1]
            ,a (True,False,True,True,True,True,True,True) [128+32+16+8+4+2+1,1]
            ,s (True,False,True,True) [128+32+16]
            ,s ((True,True,False,True,False),(False,False,True,False,True,True)) [128+64+16+1,64+32]
            ,s ('\0','\1','\127') [0,1,127]
            ,s (33::Word32,44::Word32) [33,44]
            --,s (Elem True) [64]
            --,s (NECons True (NECons False (Elem True))) [128+64+32+4]
            ,s (0::Word8) [0]
            ,s (1::Word8) [1]
            ,s (255::Word8) [255]
            ,s (0::Word16) [0]
            ,s (1::Word16) [1]
            ,s (255::Word16) [255,1]
            ,s (256::Word16) [128,2]
            ,s (65535::Word16) [255,255,3]
            ,s (255::Word32) [255,1]
            ,s (65535::Word32) [255,255,3]
            ,s (4294967295::Word32) [255,255,255,255,15]
            ,s (255::Word64) [255,1]
            ,s (65535::Word64) [255,255,3]
            ,s (4294967295::Word64) [255,255,255,255,15]
            ,s (18446744073709551615::Word64) [255,255,255,255,255,255,255,255,255,1]
            ,s (255::Word) [255,1]
            ,s (65535::Word) [255,255,3]
            ,s (4294967295::Word) [255,255,255,255,15]
            ,tstI [0::Int8,2,-2]
            ,s (127::Int8) [254]
            ,s (-128::Int8) [255]
            ,tstI [0::Int16,2,-2,127,-128]
            ,tstI [0::Int32,2,-2,127,-128]
            ,tstI [0::Int64,2,-2,127,-128]
            ,s (-1024::Int64) [255,15]
            ,tstI [0::Int,2,-2,127,-128]
            ,tstI [0::Integer,2,-2,127,-128,-256,-512]
            ,s (-1024::Integer) [255,15]
            ,s '\0' [0]
            ,s '\1' [1]
            ,s '\127' [127]
            ,s 'a' [97]
            ,s 'à' [224,1]
            ,s '经' [207,253,1]
            ,s Unit []
            ,s (Un False) [0]
            ,s (One,Two,Three) [16+8]
            --,s (NECons True (Elem True)) [128+64+16]
            ,s "" [0]
#ifdef LIST_BIT
            ,s "abc" [176,216,172,96]
            ,s [False,True,False,True] [128
                                        +32+16
                                        +8
                                        +2+1,0]
#elif defined(LIST_BYTE)
            ,s "abc" s3
            ,s (cs 600) s600
#endif
            -- Aligned structures
            ,s (T.pack "") [1,0]
            ,s (Just $ T.pack "abc") [128+1,3,97,98,99,0]
            ,s (T.pack "abc") (al s3)
            ,s (T.pack $ cs 600) (al s600)
            ,s (B.pack $ csb 3) (al c3)
            ,s (B.pack $ csb 600) (al s600)
            ,s (L.pack $ csb 3) (al c3)
            ,s (L.pack $ csb 600) (al s600)
            ]
    where
      al = (1:)
      s3 = [3,97,98,99,0]
      c3 = [3,99,99,99,0]
      s600 = concat [[255],csb 255,[255],csb 255,[90],csb 90,[0]]
      s600B = concat [[55],csb 55,[255],csb 255,[90],csb 90,[200],csb 200,[0]]
      tstI = map ti
      ti v | v >= 0    = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral v ::Word64)
           | otherwise = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral (-v) - 1 ::Word64)
      teq a b = ser a @?= ser b
      s v e = [testCase (unwords ["flat",show v]) $ ser v @?= e
              ,testCase (unwords ["unflat",show v]) $ Right v @?= des e]
      -- Aligned values unflat to the original value, modulo the added filler.
      a v e = [testCase (unwords ["flat postAligned",show v]) $ ser (postAligned v) @?= e
              ,testCase (unwords ["unflat postAligned",show v]) $ let Right (PostAligned v' _) = des e in v @?= v']
      cs n = replicate n 'c' -- take n $ cycle ['a'..'z']
      csb = map (fromIntegral . ord) . cs

uc = map ord "\x4444\x5555\x10001\xD800"

ser :: Flat a => a -> [Word8]
ser = L.unpack . flat

des :: Flat a => [Word8] -> Decoded a
des = unflat . L.pack

type RT a = a -> Bool
type RTL a = Large a -> Bool

prop_Flat_roundtrip :: (Flat a, Eq a) => a -> Bool
prop_Flat_roundtrip x = unflat (flat x) == Right x

prop_Flat_Large_roundtrip :: (Eq b, Flat b) => Large b -> Bool
prop_Flat_Large_roundtrip (Large x) = unflat (flat x) == Right x

{-
prop_common_unsigned :: (Num l,Num h,Flat l,Flat h) => l -> h -> Bool
prop_common_unsigned n _ = let n2 :: h = fromIntegral n
                           in flat n == flat n2
-}

-- e :: Stream Bool
-- e = unflatIncremental . flat $ stream1

-- el :: List Bool
-- el = unflatIncremental . flat $ infList

-- xxx = generate (arbitrary :: Gen (Large (Int)))

-- yyy = generate (arbitrary :: Gen (Word7))

-- deflat = unflat

-- f = e (preAligned True)
-- y = e (23232::Integer)
-- e = showEncoding . encode

-- x :: Get (Bool,Bool,(),Bool)
-- x = decode

-- b1 :: BLOB UTF8
-- b1 = BLOB UTF8 (preAligned (List255 [97,98,99]))
-- -- b1 = BLOB (preAligned (UTF8 (List255 [97,98,99])))

-- derive makeSerial ''Unit

instance Arbitrary B.ByteString where arbitrary   = fmap B.pack arbitrary

instance Arbitrary L.ByteString where arbitrary   = fmap L.pack arbitrary

instance Arbitrary T.Text where arbitrary   = fmap T.pack arbitrary

-- instance Arbitrary a => Arbitrary (List a) where arbitrary = fmap l2L arbitrary

derive makeArbitrary ''N

derive makeArbitrary ''Tree

derive makeArbitrary ''List

derive makeArbitrary ''Unit

derive makeArbitrary ''Un

derive makeArbitrary ''A

derive makeArbitrary ''B

-- instance Arbitrary Word7 where arbitrary  = toEnum <$> choose (0, 127)
-- derive makeArbitrary ''ASCII
