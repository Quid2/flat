{-# LANGUAGE NegativeLiterals,ScopedTypeVariables,FlexibleContexts#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.ZigZag where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.ZigZag
import Data.Word
import Data.Int
import Numeric.Natural
import Test.QuickCheck.Instances.Natural

tests :: IO TestTree
tests = testGroup "Data.ZigZag" <$> sequence [  DocTest.testProp "src/Data/ZigZag.hs:66" ( \(f::Integer) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:68" ( \(f::Natural) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:70" ( \(f::Int8) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:71" ( \(f::Word8) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:72" ( \(s::Int8) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer)) ),  DocTest.testProp "src/Data/ZigZag.hs:73" ( \(u::Word8) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural)) ),  DocTest.testProp "src/Data/ZigZag.hs:75" ( \(f::Int64) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:76" ( \(f::Word64) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:77" ( \(s::Int64) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer)) ),  DocTest.testProp "src/Data/ZigZag.hs:78" ( \(u::Word64) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural)) ),  DocTest.test "src/Data/ZigZag.hs:33" "[ExpectedLine [LineChunk \"0\"]]" (DocTest.asPrint( zigZag (0::Int8) )),  DocTest.test "src/Data/ZigZag.hs:36" "[ExpectedLine [LineChunk \"1\"]]" (DocTest.asPrint( zigZag (-1::Int16) )),  DocTest.test "src/Data/ZigZag.hs:39" "[ExpectedLine [LineChunk \"2\"]]" (DocTest.asPrint( zigZag (1::Int32) )),  DocTest.test "src/Data/ZigZag.hs:42" "[ExpectedLine [LineChunk \"3\"]]" (DocTest.asPrint( zigZag (-2::Int16) )),  DocTest.test "src/Data/ZigZag.hs:45" "[ExpectedLine [LineChunk \"99\"]]" (DocTest.asPrint( zigZag (-50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:48" "[ExpectedLine [LineChunk \"100\"]]" (DocTest.asPrint( zigZag (50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:51" "[ExpectedLine [LineChunk \"128\"]]" (DocTest.asPrint( zigZag (64::Integer) )),  DocTest.test "src/Data/ZigZag.hs:54" "[ExpectedLine [LineChunk \"511\"]]" (DocTest.asPrint( zigZag (-256::Integer) )),  DocTest.test "src/Data/ZigZag.hs:57" "[ExpectedLine [LineChunk \"512\"]]" (DocTest.asPrint( zigZag (256::Integer) )),  DocTest.test "src/Data/ZigZag.hs:60" "[ExpectedLine [LineChunk \"[5,3,1,0,2,4,6]\"]]" (DocTest.asPrint( map zigZag [-3..3::Integer] )),  DocTest.test "src/Data/ZigZag.hs:63" "[ExpectedLine [LineChunk \"[0,-1,1,-2,2,-3,3]\"]]" (DocTest.asPrint( map zagZig [0..6::Word8] ))]
