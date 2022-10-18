
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.FloatCast where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.FloatCast
import Numeric (showHex)
import Data.Word

tests :: IO TestTree
tests = testGroup "Data.FloatCast" <$> sequence [  DocTest.testProp "src/Data/FloatCast.hs:35" ( \f -> wordToFloat (floatToWord f ) == f ),  DocTest.test "src/Data/FloatCast.hs:38" "[ExpectedLine [LineChunk \"3189768192\"]]" (DocTest.asPrint( floatToWord (-0.15625) )),  DocTest.test "src/Data/FloatCast.hs:41" "[ExpectedLine [LineChunk \"-0.15625\"]]" (DocTest.asPrint( wordToFloat 3189768192 )),  DocTest.test "src/Data/FloatCast.hs:44" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( floatToWord (-5.828125) == 0xC0BA8000 )),  DocTest.testProp "src/Data/FloatCast.hs:63" ( \f -> wordToDouble (doubleToWord f ) == f ),  DocTest.test "src/Data/FloatCast.hs:66" "[ExpectedLine [LineChunk \"\\\"3ff0000000000002\\\"\"]]" (DocTest.asPrint( showHex (doubleToWord 1.0000000000000004) "" )),  DocTest.test "src/Data/FloatCast.hs:69" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( doubleToWord 1.0000000000000004 == 0x3FF0000000000002 )),  DocTest.test "src/Data/FloatCast.hs:72" "[ExpectedLine [LineChunk \"\\\"bfc4000000000000\\\"\"]]" (DocTest.asPrint( showHex (doubleToWord (-0.15625)) "" )),  DocTest.test "src/Data/FloatCast.hs:75" "[ExpectedLine [LineChunk \"-0.15625\"]]" (DocTest.asPrint( wordToDouble 0xbfc4000000000000 )),  DocTest.test "src/Data/FloatCast.hs:90" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( runST (cast (0xF0F1F2F3F4F5F6F7::Word64)) == (0xF0F1F2F3F4F5F6F7::Word64) ))]
