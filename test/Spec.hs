{-# LANGUAGE BinaryLiterals            #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NegativeLiterals          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Tests for the flat module
module Main where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Char
import           Data.DeriveTH
import           Data.Either
import           Data.Flat
import           Data.Flat.Bits
import           Data.Int
import           Data.List
import           Data.Ord
import           Data.Proxy
import qualified Data.Sequence         as Seq
import qualified Data.Text             as T
import           Data.Word
import           Numeric.Natural
import           System.Arch
import           System.Exit
import           Test.Data
import           Test.Data.Arbitrary
import           Test.Data.Flat
import           Test.Data.Values
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import System.Endian

main = do
  printInfo
  mainTest
-- main = mainShow

printInfo = do
  print getSystemArch
  print getSystemEndianness

mainShow = do
  mapM_ (\_ -> generate (arbitrary :: Gen Int) >>= print) [1..10]
  exitFailure

mainTest = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties
                           ,unitTests
                          ]

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
    ,rt "Natural" (prop_Flat_roundtrip:: RT Natural)
    ,rt "(Bool,Integer)" (prop_Flat_roundtrip:: RT (Bool,Integer))
    ,rt "Float" (prop_Flat_roundtrip:: RT Float)
    ,rt "(Bool,Float)" (prop_Flat_roundtrip:: RT (Bool,Float))
    ,rt "Double" (prop_Flat_roundtrip:: RT Double)
    ,rt "(Bool,Double)" (prop_Flat_roundtrip:: RT (Bool,Double))
    ,rt "Char" (prop_Flat_roundtrip:: RT Char)
    ,rt "(Bool,Char)" (prop_Flat_roundtrip:: RT (Bool,Char))
    --,rt "ASCII" (prop_Flat_roundtrip:: RT ASCII)
    ,rt "Unit" (prop_Flat_roundtrip:: RT Unit)
    ,rt "Un" (prop_Flat_roundtrip:: RT Un )
    ,rt "N" (prop_Flat_roundtrip:: RT N )
    ,rt "A" (prop_Flat_roundtrip:: RT A )
    ,rt "B" (prop_Flat_roundtrip:: RT B )
    ,rt "Maybe N" (prop_Flat_roundtrip:: RT (Maybe N))
    ,rt "Either N Bool" (prop_Flat_roundtrip:: RT (Either N Bool))
    ,rt "Either Int Char" (prop_Flat_roundtrip:: RT (Either Int Char))
    -- ,rt "Tree Bool" (prop_Flat_roundtrip:: RT (Tree Bool))
    -- ,rt "Tree N" (prop_Flat_roundtrip:: RT (Tree N))
    ,rt "List N" (prop_Flat_roundtrip:: RT (List N))
    ,rt "[Int16]" (prop_Flat_roundtrip:: RT [Int16])
    ,rt "String" (prop_Flat_roundtrip:: RT String)
    -- Generates incorrect ascii chars?
    ,rt "Text" (prop_Flat_roundtrip:: RT T.Text)
    ,rt "ByteString" (prop_Flat_roundtrip:: RT B.ByteString)
    ,rt "Lazy ByteString" (prop_Flat_roundtrip:: RT L.ByteString)
    ,rt "Short ByteString" (prop_Flat_roundtrip:: RT SBS.ShortByteString)
  ]
   where rt n = QC.testProperty (unwords ["round trip",n])

instance Flat [Int16]
instance Flat [Word8]

unitTests = testGroup "De/Serialisation Unit tests" $ concat [
  sz () 0
  ,sz True 1
  ,sz One 2
  ,sz Two 2
  ,sz Three 2
  ,sz Four 3
  ,sz Five 3
  ,sz 'a' 8
  ,sz 'à' 16
  ,sz '经' 24
  ,sz (0::Word8) 8
  ,sz (1::Word8) 8
  ,concat $ map (uncurry sz) $ ns
  ,concat $ map (uncurry sz) $ nsI
  ,concat $ map (uncurry sz) $ nsII
  ,sz (1.1::Float) 32
  ,sz (1.1::Double) 64
  ,sz "" 1
  ,sz "abc" (4+3*8)
  ,sz ((),(),Unit) 0
  ,sz (True,False,One,Five) 7
  ,sz bs (4+3*8)
  ,sz stBS bsSize
  ,sz lzBS bsSize
  ,sz shBS bsSize
  ,sz tx utf16Size
  ,errDec (Proxy::Proxy Bool) [] -- no data
  ,errDec (Proxy::Proxy Bool) [128] -- no filler
  ,errDec (Proxy::Proxy Bool) [128+1,1,2,4,8] -- additional bytes
  ,s () []
  ,s ((),(),Unit) []
  ,s (Unit,'a',Unit,'a',Unit,'a',Unit) [97,97,97]
  ,a () [1]
  ,a True [128+1]
  ,a (True,True) [128+64+1]
  ,a (True,False,True) [128+32+1]
  ,a (True,False,True,True) [128+32+16+1]
  ,a (True,False,True,True,True) [128+32+16+8+1]
  ,a (True,False,True,True,True,True) [128+32+16+8+4+1]
  ,a (True,False,True,True,True,True,True) [128+32+16+8+4+2+1]
  ,a (True,False,True,True,(True,True,True,True)) [128+32+16+8+4+2+1,1]
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
  ,s (127::Word32) [127]
  ,s (128::Word32) [128,1]
  ,s (129::Word32) [129,1]
  ,s (255::Word32) [255,1]
  ,s (16383::Word32) [255,127]
  ,s (16384::Word32) [128,128,1]
  ,s (16385::Word32) [129,128,1]
  ,s (32767::Word32) [255,255,1]
  ,s (32768::Word32) [128,128,2]
  ,s (32769::Word32) [129,128,2]
  ,s (65535::Word32) [255,255,3]
  ,s (2097151::Word32) [255,255,127]
  ,s (2097152::Word32) [128,128,128,1]
  ,s (2097153::Word32) [129,128,128,1]
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
  ,s (-0.15625::Float)  [0b10111110,0b00100000,0,0]
  ,s (-0.15625::Double) [0b10111111,0b11000100,0,0,0,0,0,0]
  ,s (-123.2325E-23::Double) [0b10111011,0b10010111,0b01000111,0b00101000,0b01110101,0b01111011,0b01000111,0b10111010]
  ,map trip [0::Float,-0::Float,0/0::Float,1/0::Float]
  ,map trip [0::Double,-0::Double,0/0::Double,1/0::Double]
  ,s '\0' [0]
  ,s '\1' [1]
  ,s '\127' [127]
  ,s 'a' [97]
  ,s 'à' [224,1]
  ,s '经' [207,253,1]
  ,[trip [chr 0x10FFFF]]
  ,s Unit []
  ,s (Un False) [0]
  ,s (One,Two,Three) [16+8]
  ,s (Five,Five,Five) [255,128]
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
    --,s (T.pack "") [1,0]
    --,s (Just $ T.pack "abc") [128+1,3,97,98,99,0]
    --,s (T.pack "abc") (al s3)
    --,s (T.pack $ cs 600) (al s600)
  ,s (B.pack $ csb 3) (bsl c3)
  ,s (B.pack $ csb 600) (bsl s600)
  ,s (L.pack $ csb 3) (bsl c3)
   -- Long LazyStrings can have internal sections shorter than 255
   --,s (L.pack $ csb 600) (bsl s600)
  ,[trip [1..100::Int16]]
  ,[trip unicodeText]
  ,[trip longBS,trip longLBS,trip longSBS]
  ,[trip longSeq]
  ,[trip mapV]
  ]
    where
      ns :: [(Word64, Int)]
      ns =  [( (-) (2 ^(i*7)) 1,fromIntegral (8*i)) | i <- [1 .. 10]]

      nsI :: [(Int64, Int)]
      nsI = nsI_
      nsII :: [(Integer, Int)]
      nsII = nsI_
      nsI_ =  [( (-) (2 ^(((-) i 1)*7)) 1,fromIntegral (8*i)) | i <- [1 .. 10]]

      --al = (1:) -- prealign
      bsl = id -- noalign
      s3 = [3,97,98,99,0]
      c3a = [3,99,99,99,0] -- Array Word8
      c3 = pre c3a
      s600 = pre s600a
      pre = (1:)
      tx = T.pack "txt"
      utf16Size = 8+8+3*16+8
      shBS = SBS.toShort stBS
      lzBS = L.pack bs
      stBS = B.pack bs
      bs = [32,32,32::Word8]
      bsSize = 8+8+3*8+8
      s600a = concat [[255],csb 255,[255],csb 255,[90],csb 90,[0]]
      s600B = concat [[55],csb 55,[255],csb 255,[90],csb 90,[200],csb 200,[0]]
      longSeq :: Seq.Seq Word8
      longSeq = Seq.fromList lbs
      longSBS = SBS.toShort longBS
      longBS = B.pack lbs
      longLBS = L.concat $ concat $ replicate 10 [L.pack lbs]
      lbs = concat $ replicate 100 [234,123,255,0]
      tstI = map ti

      ti v | v >= 0    = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral v ::Word64)
           | otherwise = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral (-v) - 1 ::Word64)

      teq a b = ser a @?= ser b

      sz v e = [testCase (unwords ["size of",sshow v]) $ getSize v @?= e]

      s v e = [testCase (unwords ["flat raw",sshow v]) $ serRaw v @?= e
              ,testCase (unwords ["unflat raw",sshow v]) $ desRaw e @?= Right v]

      -- Aligned values unflat to the original value, modulo the added filler.
      a v e = [testCase (unwords ["flat",sshow v]) $ ser v @?= e
              ,testCase (unwords ["unflat",sshow v]) $ let Right v' = des e in v @?= v']
      -- a v e = [testCase (unwords ["flat postAligned",show v]) $ ser (postAligned v) @?= e
      --         ,testCase (unwords ["unflat postAligned",show v]) $ let Right (PostAligned v' _) = des e in v @?= v']
      cs n = replicate n 'c' -- take n $ cycle ['a'..'z']
      csb = map (fromIntegral . ord) . cs
      sshow = take 80 . show

      trip :: forall a .(Show a,Flat a) => a -> TestTree
      trip v = testCase (unwords ["roundtrip",sshow v]) $ show (unflat (flat v)::Decoded a) @?= show (Right v::Decoded a) -- we use show to get Right NaN == Right NaN

errDec :: forall a . (Flat a, Eq a, Show a) => Proxy a -> [Word8] -> [TestTree]
--errDec _ bs = [testCase "bad decode" $ let ev = (des bs::Decoded a) in ev @?= Left ""]
errDec _ bs = [testCase "bad decode" $ let ev = (des bs::Decoded a) in isRight ev @?= False]

uc = map ord "\x4444\x5555\x10001\xD800"

ser :: Flat a => a -> [Word8]
ser = L.unpack . flat

des :: Flat a => [Word8] -> Decoded a
des = unflat . L.pack

serRaw :: Flat a => a -> [Word8]
--serRaw = L.unpack . flatRaw
serRaw = asBytes . valueBits

desRaw :: Flat a => [Word8] -> Decoded a
desRaw = unflatRaw . L.pack

type RT a = a -> Bool
type RTL a = Large a -> Bool

prop_Flat_roundtrip :: (Flat a, Eq a) => a -> Bool
prop_Flat_roundtrip = rtrip2

prop_Flat_Large_roundtrip :: (Eq b, Flat b) => Large b -> Bool
prop_Flat_Large_roundtrip (Large x) = rtrip2 x

rtrip x = unflat (flat x) == Right x
rtrip2 x = rtrip x && rtrip (True,x,False)

{-
prop_common_unsigned :: (Num l,Num h,Flat l,Flat h) => l -> h -> Bool
prop_common_unsigned n _ = let n2 :: h = fromIntegral n
                           in flat n == flat n2
-}

-- e :: Stream Bool
-- e = unflatIncremental . flat $ stream1

-- el :: List Bool
-- el = unflatIncremental . flat $ infList

-- deflat = unflat

-- b1 :: BLOB UTF8
-- b1 = BLOB UTF8 (preAligned (List255 [97,98,99]))
-- -- b1 = BLOB (preAligned (UTF8 (List255 [97,98,99])))

