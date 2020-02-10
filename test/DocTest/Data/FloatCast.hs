
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.FloatCast where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.FloatCast
import Numeric (showHex)

tests :: IO TestTree

tests = testGroup "Data.FloatCast" <$> sequence [  DocTest.testProp "src/Data/FloatCast.hs:39" ( \f -> wordToFloat (floatToWord f ) == f ),  DocTest.test "src/Data/FloatCast.hs:41" ["3189768192"] (DocTest.asPrint( floatToWord (-0.15625) )),  DocTest.test "src/Data/FloatCast.hs:44" ["-0.15625"] (DocTest.asPrint( wordToFloat 3189768192 )),  DocTest.test "src/Data/FloatCast.hs:47" ["True"] (DocTest.asPrint( floatToWord (-5.828125) == 0xC0BA8000 )),  DocTest.testProp "src/Data/FloatCast.hs:66" ( \f -> wordToDouble (doubleToWord f ) == f ),  DocTest.test "src/Data/FloatCast.hs:68" ["\"3ff0000000000002\""] (DocTest.asPrint( showHex (doubleToWord 1.0000000000000004) "" )),  DocTest.test "src/Data/FloatCast.hs:71" ["True"] (DocTest.asPrint( doubleToWord 1.0000000000000004 == 0x3FF0000000000002 )),  DocTest.test "src/Data/FloatCast.hs:74" ["13818169556679524352"] (DocTest.asPrint( doubleToWord (-0.15625) )),  DocTest.test "src/Data/FloatCast.hs:77" ["-0.15625"] (DocTest.asPrint( wordToDouble 13818169556679524352 )),  DocTest.test "src/Data/FloatCast.hs:101" ["True"] (DocTest.asPrint( runST (cast (0xF0F1F2F3F4F5F6F7::Word64)) == (0xF0F1F2F3F4F5F6F7::Word64) ))]
