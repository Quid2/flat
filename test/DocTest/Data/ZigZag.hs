{-# LANGUAGE NegativeLiterals,ScopedTypeVariables,FlexibleContexts#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.ZigZag where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.ZigZag
import Data.Word
import Data.Int
import Numeric.Natural
import Test.QuickCheck.Arbitrary
instance Arbitrary Natural where arbitrary = arbitrarySizedNatural; shrink    = shrinkIntegral

tests :: IO TestTree
tests = testGroup "Data.ZigZag" <$> sequence [  DocTest.testProp "src/Data/ZigZag.hs:67" ( \(f::Integer) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:70" ( \(f::Natural) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:73" ( \(f::Int8) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:76" ( \(f::Word8) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:79" ( \(s::Int8) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer)) ),  DocTest.testProp "src/Data/ZigZag.hs:82" ( \(u::Word8) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural)) ),  DocTest.testProp "src/Data/ZigZag.hs:85" ( \(f::Int64) -> zagZig (zigZag f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:88" ( \(f::Word64) -> zigZag (zagZig f) == f ),  DocTest.testProp "src/Data/ZigZag.hs:91" ( \(s::Int64) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer)) ),  DocTest.testProp "src/Data/ZigZag.hs:94" ( \(u::Word64) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural)) ),  DocTest.test "src/Data/ZigZag.hs:34" "[ExpectedLine [LineChunk \"0\"]]" (DocTest.asPrint( zigZag (0::Int8) )),  DocTest.test "src/Data/ZigZag.hs:37" "[ExpectedLine [LineChunk \"1\"]]" (DocTest.asPrint( zigZag (-1::Int16) )),  DocTest.test "src/Data/ZigZag.hs:40" "[ExpectedLine [LineChunk \"2\"]]" (DocTest.asPrint( zigZag (1::Int32) )),  DocTest.test "src/Data/ZigZag.hs:43" "[ExpectedLine [LineChunk \"3\"]]" (DocTest.asPrint( zigZag (-2::Int16) )),  DocTest.test "src/Data/ZigZag.hs:46" "[ExpectedLine [LineChunk \"99\"]]" (DocTest.asPrint( zigZag (-50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:49" "[ExpectedLine [LineChunk \"100\"]]" (DocTest.asPrint( zigZag (50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:52" "[ExpectedLine [LineChunk \"128\"]]" (DocTest.asPrint( zigZag (64::Integer) )),  DocTest.test "src/Data/ZigZag.hs:55" "[ExpectedLine [LineChunk \"511\"]]" (DocTest.asPrint( zigZag (-256::Integer) )),  DocTest.test "src/Data/ZigZag.hs:58" "[ExpectedLine [LineChunk \"512\"]]" (DocTest.asPrint( zigZag (256::Integer) )),  DocTest.test "src/Data/ZigZag.hs:61" "[ExpectedLine [LineChunk \"[5,3,1,0,2,4,6]\"]]" (DocTest.asPrint( map zigZag [-3..3::Integer] )),  DocTest.test "src/Data/ZigZag.hs:64" "[ExpectedLine [LineChunk \"[0,-1,1,-2,2,-3,3]\"]]" (DocTest.asPrint( map zagZig [0..6::Word8] ))]
