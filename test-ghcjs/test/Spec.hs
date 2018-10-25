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

import           Data.Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Char
import           Data.Either
import           Data.Flat
import           Data.Flat.Bits
import           Data.Flat.Decoder
import qualified Data.Flat.Encoder       as E
import qualified Data.Flat.Encoder.Strict       as E
import qualified Data.Flat.Encoder.Prim       as E
import           Data.Int
import           Data.List
import qualified Data.Map              as M
import           Data.Ord
import           Data.Proxy
import qualified Data.Sequence         as Seq
import qualified Data.Text             as T
import           Data.Word
import           Numeric.Natural
import Control.Monad
import           System.Exit
import           Test.Data
import           Test.Data.Arbitrary
import           Test.Data.Flat
import           Test.Data.Values
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC hiding (getSize)
-- import           System.Arch
-- import           System.Endian
import Data.FloatCast

main = do
  -- printInfoqqq
  mainTest
  -- print $ flatRaw 18446744073709551615::Word64
  -- print $ B.unpack . flat $ (True,0::Word64,18446744073709551615::Word64)
  -- print (2^56::Word64,fromIntegral (1::Word8) `shiftL` 56 :: Word64,(18446744073709551615::Word64) `shiftR` 1)
  -- mainShow
  -- eWord64E id 0b

-- printInfo = do
--   print $ "BigEndian: " ++ show isBigEndian
--    print getSystemArch
--    print getSystemEndianness

mainShow = do
  mapM_ (\_ -> generate (arbitrary :: Gen Int) >>= print) [1..10]
  exitFailure

mainTest = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basicTests,flatTests,properties
                          ]

properties = testGroup "Properties"
  [  rt "()" (prop_Flat_roundtrip:: RT ())
    ,rt "Bool" (prop_Flat_roundtrip::RT Bool)
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
    ,rt "Float" (prop_Flat_roundtrip:: RT Float)
    ,rt "Double" (prop_Flat_roundtrip:: RT Double)
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
    -- ,rt "Tree Bool" (prop_Flat_roundtrip:: RT (Tree Bool))
    -- ,rt "Tree N" (prop_Flat_roundtrip:: RT (Tree N))
    ,rt "List N" (prop_Flat_roundtrip:: RT (List N))
    ,rt "[Int16]" (prop_Flat_roundtrip:: RT [Int16])
    ,rt "String" (prop_Flat_roundtrip:: RT String)
    -- Generates incorrect ascii chars?
    ,rt "Text" (prop_Flat_roundtrip:: RT T.Text)
    ,rt "ByteString" (prop_Flat_roundtrip:: RT B.ByteString)
    ,rt "Lazy ByteString" (prop_Flat_roundtrip:: RT L.ByteString)
#ifndef ghcjs_HOST_OS
    ,rt "Short ByteString" (prop_Flat_roundtrip:: RT SBS.ShortByteString)
#endif
    ,rt "floatToWord" (prop_float_conv :: RT Float)
    ,rt "doubleToWord" (prop_double_conv :: RT Double)
    ]
   where rt n = QC.testProperty (unwords ["round trip",n])

instance Flat [Int16]
instance Flat [Word8]
instance Flat [Bool]

basicTests = testGroup "Basic tests" [] -- testShifts

-- see: https://github.com/ghcjs/ghcjs/issues/706
testShifts = map tst [0..33] 
  where 
   tst n = testCase ("shiftR " ++ show n) $ 
    let val = 4294967295::Word32
        s = val `shift` (-n)
        r = val `shiftR` n
    in r @?= s

-- shR = shiftR
-- shR = unsafeShiftR
shR val 0 = val
shR val n = shift val (-n)

flatTests = testGroup "De/Serialisation Unit tests" $ concat [
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
  ,concatMap (uncurry sz) ns
  ,concatMap (uncurry sz) nsI
  ,concatMap (uncurry sz) nsII
  ,sz (1.1::Float) 32
  ,sz (1.1::Double) 64
  ,sz "" 1
  ,sz "abc" (4+3*8)
  ,sz ((),(),Unit) 0
  ,sz (True,False,One,Five) 7
  ,sz map1 7
  ,sz bs (4+3*8)
  ,sz stBS bsSize
  ,sz lzBS bsSize
#ifndef ghcjs_HOST_OS
  ,sz shBS bsSize
#endif      
  ,sz tx utf8Size
  ,sz (UTF8Text tx) utf8Size
  ,sz (UTF16Text tx) utf16Size

  -- Expected errors
  ,errDec (Proxy::Proxy Bool) [] -- no data
  ,errDec (Proxy::Proxy Bool) [128] -- no filler
  ,errDec (Proxy::Proxy Bool) [128+1,1,2,4,8] -- additional bytes

  ,encRaw () []
  ,encRaw ((),(),Unit) []
  ,encRaw (Unit,'a',Unit,'a',Unit,'a',Unit) [97,97,97]
  ,a () [1]
  ,a True [128+1]
  ,a (True,True) [128+64+1]
  ,a (True,False,True) [128+32+1]
  ,a (True,False,True,True) [128+32+16+1]
  ,a (True,False,True,True,True) [128+32+16+8+1]
  ,a (True,False,True,True,True,True) [128+32+16+8+4+1]
  ,a (True,False,True,True,True,True,True) [128+32+16+8+4+2+1]
  ,a (True,False,True,True,(True,True,True,True)) [128+32+16+8+4+2+1,1]
  ,encRaw (True,False,True,True) [128+32+16]
  ,encRaw ((True,True,False,True,False),(False,False,True,False,True,True)) [128+64+16+1,64+32]
  ,encRaw ('\0','\1','\127') [0,1,127]
  ,encRaw (33::Word32,44::Word32) [33,44]
    --,s (Elem True) [64]
    --,s (NECons True (NECons False (Elem True))) [128+64+32+4]
  ,encRaw (0::Word8) [0]
  ,encRaw (1::Word8) [1]
  ,encRaw (255::Word8) [255]
  ,encRaw (0::Word16) [0]
  ,encRaw (1::Word16) [1]
  ,encRaw (255::Word16) [255,1]
  ,encRaw (256::Word16) [128,2]
  ,encRaw (65535::Word16) [255,255,3]
  ,encRaw (127::Word32) [127]
  ,encRaw (128::Word32) [128,1]
  ,encRaw (129::Word32) [129,1]
  ,encRaw (255::Word32) [255,1]
  ,encRaw (16383::Word32) [255,127]
  ,encRaw (16384::Word32) [128,128,1]
  ,encRaw (16385::Word32) [129,128,1]
  ,encRaw (32767::Word32) [255,255,1]
  ,encRaw (32768::Word32) [128,128,2]
  ,encRaw (32769::Word32) [129,128,2]
  ,encRaw (65535::Word32) [255,255,3]
  ,encRaw (2097151::Word32) [255,255,127]
  ,encRaw (2097152::Word32) [128,128,128,1]
  ,encRaw (2097153::Word32) [129,128,128,1]
  ,encRaw (4294967295::Word32) [255,255,255,255,15]
  ,encRaw (255::Word64) [255,1]
  ,encRaw (65535::Word64) [255,255,3]
  ,encRaw (4294967295::Word64) [255,255,255,255,15]
  ,encRaw (18446744073709551615::Word64)       [255,255,255,255,255,255,255,255,255,1]
  ,encRaw (False,18446744073709551615::Word64) [127,255,255,255,255,255,255,255,255,128,128]
  ,encRaw (255::Word) [255,1]
  ,encRaw (65535::Word) [255,255,3]
  ,encRaw (4294967295::Word) [255,255,255,255,15]
  ,tstI [0::Int8,2,-2]
  ,encRaw (127::Int8) [254]
  ,encRaw (-128::Int8) [255]
  ,tstI [0::Int16,2,-2,127,-128]
  ,tstI [0::Int32,2,-2,127,-128]
  ,tstI [0::Int64,2,-2,127,-128]
  ,encRaw (-1024::Int64) [255,15]
  ,encRaw (maxBound::Word8)       [255]
  ,encRaw (True,maxBound::Word8)  [255,128]
  ,encRaw (maxBound::Word16)      [255,255,3]
  ,encRaw (True,maxBound::Word16) [255,255,129,128]
  ,encRaw (maxBound::Word32)      [255,255,255,255,15]
  ,encRaw (True,maxBound::Word32) [255,255,255,255,135,128]
  ,encRaw (maxBound::Word64)      [255,255,255,255,255,255,255,255,255,1]
  ,encRaw (True,maxBound::Word64) [255,255,255,255,255,255,255,255,255,128,128]
  ,encRaw (minBound::Int64) [255,255,255,255,255,255,255,255,255,1]
  ,encRaw (maxBound::Int64) [254,255,255,255,255,255,255,255,255,1]
  ,tstI [0::Int,2,-2,127,-128]
  ,tstI [0::Integer,2,-2,127,-128,-256,-512]
  ,encRaw (-1024::Integer) [255,15]
  ,encRaw (0::Float)  [0,0,0,0]
  ,encRaw (-2::Float) [0b11000000,0,0,0]
  ,encRaw (0.085::Float) [0b00111101,0b10101110,0b00010100,0b01111011]
  ,encRaw (0::Double)  [0,0,0,0,0,0,0,0]
  ,encRaw (-2::Double) [0b11000000,0,0,0,0,0,0,0]
  ,encRaw (23::Double) [0b01000000,0b00110111,0,0,0,0,0,0]
  ,encRaw (-0.15625::Float)  [0b10111110,0b00100000,0,0]
  ,encRaw (-0.15625::Double) [0b10111111,0b11000100,0,0,0,0,0,0]
  ,encRaw (-123.2325E-23::Double) [0b10111011,0b10010111,0b01000111,0b00101000,0b01110101,0b01111011,0b01000111,0b10111010]
  ,encRaw (Left True :: Either Bool (Double, Double)) [0b01000000]
  ,encRaw (-2.1234E15 :: Double) [195,30,44,226,90,221,64,0]
  ,encRaw (1.1234E-22 :: Double) [59,96,249,241,120,219,249,174]
  ,encRaw ((False,-2.1234E15) :: (Bool,Double)) [97,143,22,113,45,110,160,0,0]
  ,encRaw ((True,-2.1234E15) :: (Bool,Double)) [225,143,22,113,45,110,160,0,0]
  ,encRaw ((-2.1234E15 , 1.1234E-22) :: (Double, Double)) $ [0b11000011,30,44,226,90,221,64,0] ++ [59,96,249,241,120,219,249,174]
  ,encRaw ((True,-2.1234E15 , 1.1234E-22) :: (Bool,Double, Double)) [0b11100001,143,22,113,45,110,160,0,29,176,124,248,188,109,252,215,0]
  ,encRaw (Right (-2.1234E15 , 1.1234E-22) :: Either Bool (Double, Double)) [0b11100001,143,22,113,45,110,160,0,29,176,124,248,188,109,252,215,0]
  ,encRaw (Left True:: Either Bool Direction) [0b01000000]
  ,encRaw (Right West :: Either Bool Direction) [0b11110000]
  ,dec ((,,,) <$> dropBits 13 <*> dBool <*> dBool <*> dBool) [0b10111110,0b10011010] ((),False,True,False)
  ,dec ((,,,) <$> dropBits 1 <*> dBE16 <*> dBool <*> dropBits 6) [0b11000000
                                                                 ,0b00000001
                                                                 ,0b01000000] ((),2^15+2,True,())
  ,dec ((,,,) <$> dropBits 1 <*> dBE32 <*> dBool <*> dropBits 6) [0b11000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000001
                                                                 ,0b01000000] ((),2^31+2,True,())
  ,dec (dBE64) [0b10000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000010
                                                                 ] (2^63+2)

  ,dec ((,,,) <$> dropBits 1 <*> dBE64 <*> dBool <*> dropBits 6) [0b11000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000000
                                                                 ,0b00000001
                                                                 ,0b01000000] ((),2^63+2,True,())

  ,decBitsN dBEBits8
  ,decBitsN dBEBits16
  ,decBitsN dBEBits32
  ,decBitsN dBEBits64

  -- System.Endian tests (to run, need to modify imports and cabal file)
  -- ,conv toBE16 (2^10 + 3)  (2^9+2^8+4)
  -- ,conv toBE32 (2^18 + 3)  50332672
  -- ,conv toBE64 (2^34 + 3)  216172782180892672

  ,conv floatToWord -0.15625 3189768192
  ,conv wordToFloat 3189768192 -0.15625 
  ,conv doubleToWord -0.15625 13818169556679524352
  ,conv wordToDouble 13818169556679524352 -0.15625 

  ,encRawWith 1 E.eTrueF [0b10000001]
  ,encRawWith 3 (E.eTrueF >=> E.eFalseF >=> E.eTrueF) [0b10100001]
  
  ,encRawWith 32 (E.eWord32E id $ 2^18 + 3) [3,0,4,0,1]
  ,encRawWith 32 (E.eWord32BEF  $ 2^18 + 3) [0,4,0,3,1]

  ,encRawWith 64 (E.eWord64E id $ 2^34 + 3) [3,0,0,0,4,0,0,0,1]
  ,encRawWith 64 (E.eWord64BEF  $ 2^34 + 3) [0,0,0,4,0,0,0,3,1]
  ,encRawWith 65 (E.eTrueF >=> E.eWord64E id (2^34 + 3)) [1,0,0,0,2,0,0,128,129]
  ,encRawWith 65 (E.eTrueF >=> E.eWord64BEF (2^34 + 3)) [128,0,0,2,0,0,0,1,129]
  ,encRawWith 65 (E.eFalseF >=> E.eWord64E id (2^34 + 3)) [1,0,0,0,2,0,0,0,129]
  ,encRawWith 65 (E.eFalseF >=> E.eWord64BEF (2^34 + 3))  [0,0,0,2,0,0,0,1,129]

  ,map trip [minBound,maxBound::Word8]
  ,map trip [minBound,maxBound::Word16]
  ,map trip [minBound,maxBound::Word32]
  ,map trip [minBound,maxBound::Word64]
  ,map trip [minBound::Int8,maxBound::Int8]
  ,map trip [minBound::Int16,maxBound::Int16]
  ,map trip [minBound::Int32,maxBound::Int32]
  ,map trip [minBound::Int64,maxBound::Int64]
  ,map trip [0::Float,-0::Float,0/0::Float,1/0::Float]
  ,map trip [0::Double,-0::Double,0/0::Double,1/0::Double]
  ,encRaw '\0' [0]
  ,encRaw '\1' [1]
  ,encRaw '\127' [127]
  ,encRaw 'a' [97]
  ,encRaw 'à' [224,1]
  ,encRaw '经' [207,253,1]
  ,[trip [chr 0x10FFFF]]
  ,encRaw Unit []
  ,encRaw (Un False) [0]
  ,encRaw (One,Two,Three) [16+8]
  ,encRaw (Five,Five,Five) [255,128]
    --,s (NECons True (Elem True)) [128+64+16]
  ,encRaw "" [0]
#ifdef LIST_BIT
  ,encRaw "abc" [176,216,172,96]
  ,encRaw [False,True,False,True] [128
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
  ,encRaw map1 [0b10111000]
  ,encRaw (B.pack $ csb 3) (bsl c3)
  ,encRaw (B.pack $ csb 600) (bsl s600)
  ,encRaw (L.pack $ csb 3) (bsl c3)
   -- Long LazyStrings can have internal sections shorter than 255
   --,s (L.pack $ csb 600) (bsl s600)
  ,[trip [1..100::Int16]]
  ,[trip asciiStrT,trip "维护和平正",trip (T.pack "abc"),trip unicodeText,trip unicodeTextUTF8T]
  ,[trip longBS,trip longLBS]
#ifndef ghcjs_HOST_OS
  ,[trip longSBS]
  ,[trip unicodeTextUTF16T]
#endif
  ,[trip longSeq]
  ,[trip mapV]
  ,[trip map1]
  ]
    where
      map1 = M.fromList [(False,True),(True,False)]

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
      utf8Size = 8+8+3*32+8
      utf16Size = 8+8+3*16+8
      lzBS = L.pack bs
      stBS = B.pack bs
      bs = [32,32,32::Word8]
      bsSize = 8+8+3*8+8
      s600a = concat [[255],csb 255,[255],csb 255,[90],csb 90,[0]]
      s600B = concat [[55],csb 55,[255],csb 255,[90],csb 90,[200],csb 200,[0]]
      longSeq :: Seq.Seq Word8
      longSeq = Seq.fromList lbs
      longBS = B.pack lbs
      longLBS = L.concat $ concat $ replicate 10 [L.pack lbs]
#ifndef ghcjs_HOST_OS      
      shBS = SBS.toShort stBS
      longSBS = SBS.toShort longBS
#endif      
      lbs = concat $ replicate 100 [234,123,255,0]
      tstI = map ti

      ti v | v >= 0    = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral v ::Word64)
           | otherwise = testCase (unwords ["Int",show v]) $ teq v (2 * fromIntegral (-v) - 1 ::Word64)

      teq a b = ser a @?= ser b

      sz v e = [testCase (unwords ["size of",sshow v]) $ getSize v @?= e]

      conv f v e = [testCase (unwords ["conv",sshow v,showB . flat $ v,"to",sshow e]) $ f v @?= e]

      encRaw :: forall a. (Show a, Flat a) => a -> [Word8] -> [TestTree]
      encRaw v e = [testCase (unwords ["flat raw",sshow v,show . B.unpack . flat $ v]) $ serRaw v @?= e]
              --,testCase (unwords ["unflat raw",sshow v]) $ desRaw e @?= Right v]

      encRawWith sz enc exp = [testCase (unwords ["encode raw with size",show sz]) $ flatRawWith sz enc @?= exp] 

      dec decOp v e = [testCase (unwords ["decode",sshow v]) $ unflatRawWith decOp (B.pack v) @?= Right e]

      -- Test dBEBits8/16/32/64, extraction of up to 8/16/32/bits from various positions
      decBitsN :: forall a. (Num a,FiniteBits a,Show a,Flat a) => (Int -> Get a) -> [TestTree]
      decBitsN dec = let s = finiteBitSize (undefined::a)
                     in [decBits_ dec val numBitsToTake pre | numBitsToTake <- [0 .. s], val <- [0::a ,1+2^(s - 2)+2^(s - 5) ,fromIntegral $ (2^s::Integer) - 1],pre <- [0,1,7]]

      decBits_ :: forall a. (FiniteBits a,Show a,Flat a) => (Int -> Get a) -> a -> Int -> Int -> TestTree
      decBits_ deco val numBitsToTake pre =
        -- a sequence composed by pre zero bits followed by the val and zero bits till the next byte boundary
        let vs = B.pack . asBytes . fromBools $ replicate pre False ++ toBools (asBits val)
            len = B.length vs
            sz = finiteBitSize (undefined::a)
            dec :: Get a
            dec = do
              dropBits pre
              r <- deco numBitsToTake
              dropBits (len*8-numBitsToTake-pre)
              return r
            -- we expect the first numBitsToTake bits of the value
            expectedD@(Right expected) :: Decoded a = Right $ val `shR` (sz - numBitsToTake) -- ghcjs: shiftR fails, see: https://github.com/ghcjs/ghcjs/issues/706
            actualD@(Right actual) :: Decoded a = unflatRawWith dec vs
        in testCase (unwords ["take",show numBitsToTake,"bits from",show val,"of size",show sz,"with prefix",show pre,"sequence",showB vs,show expected,show actual,show $ val == actual,show $ expected == actual,show $ expected /= actual,show $ show expected == show actual,show $ flat expected == flat actual]) 
            $ actualD @?= expectedD

      -- Aligned values unflat to the original value, modulo the added filler.
      a v e = [testCase (unwords ["flat",sshow v]) $ ser v @?= e
              ,testCase (unwords ["unflat",sshow v]) $ let Right v' = des e in v @?= v']
      -- a v e = [testCase (unwords ["flat postAligned",show v]) $ ser (postAligned v) @?= e
      --         ,testCase (unwords ["unflat postAligned",show v]) $ let Right (PostAligned v' _) = des e in v @?= v']
      cs n = replicate n 'c' -- take n $ cycle ['a'..'z']
      csb = map (fromIntegral . ord) . cs
      sshow = take 80 . show
       
      trip :: forall a .(Show a,Flat a) => a -> TestTree
      trip v = testCase (unwords ["roundtrip",sshow v]) $
        -- we use show to get Right NaN == Right NaN 
        show (unflat (flat v::B.ByteString)::Decoded a) @?= show (Right v::Decoded a)

showB = show . B.unpack 

errDec :: forall a . (Flat a, Eq a, Show a) => Proxy a -> [Word8] -> [TestTree]
--errDec _ bs = [testCase "bad decode" $ let ev = (des bs::Decoded a) in ev @?= Left ""]
errDec _ bs = [testCase "bad decode" $ let ev = (des bs::Decoded a) in isRight ev @?= False]

ser :: Flat a => a -> [Word8]
ser = B.unpack . flat

des :: Flat a => [Word8] -> Decoded a
des = unflat

flatRawWith sz enc = B.unpack $ E.strictEncoder (sz+8) (E.Encoding $ enc >=> E.eFillerF) 

serRaw :: Flat a => a -> [Word8]
-- serRaw = B.unpack . flatRaw
-- serRaw = L.unpack . flatRaw
serRaw = asBytes . bits

--desRaw :: Flat a => [Word8] -> Decoded a
--desRaw = unflatRaw . L.pack

type RT a = a -> Bool
type RTL a = Large a -> Bool

prop_Flat_roundtrip :: (Flat a, Eq a) => a -> Bool
prop_Flat_roundtrip = roundTripExt

prop_Flat_Large_roundtrip :: (Eq b, Flat b) => Large b -> Bool
prop_Flat_Large_roundtrip (Large x) = roundTripExt x

roundTrip x = unflat (flat x::B.ByteString) == Right x

-- Test roundtrip for both the value and the value embedded between bools
roundTripExt x = roundTrip x && roundTrip (True,x,False)

prop_double_conv d = wordToDouble (doubleToWord d) == d 

prop_float_conv d = wordToFloat (floatToWord d) == d 

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




