{-# LANGUAGE NegativeLiterals,ScopedTypeVariables#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.ZigZag where
import qualified DocTest
import Test.Tasty(testGroup)
import Data.ZigZag

tests = testGroup "Data.ZigZag" <$> sequence [  DocTest.test "src/Data/ZigZag.hs:39" ["0"] (DocTest.asPrint( zzEncodeInteger (0::Integer) )),  DocTest.test "src/Data/ZigZag.hs:42" ["1"] (DocTest.asPrint( zzEncodeInteger (-1::Integer) )),  DocTest.test "src/Data/ZigZag.hs:45" ["2"] (DocTest.asPrint( zzEncodeInteger (1::Integer) )),  DocTest.test "src/Data/ZigZag.hs:48" ["3"] (DocTest.asPrint( zzEncodeInteger (-2::Integer) )),  DocTest.test "src/Data/ZigZag.hs:51" ["4"] (DocTest.asPrint( zzEncodeInteger (2::Integer) )),  DocTest.test "src/Data/ZigZag.hs:54" ["99"] (DocTest.asPrint( zzEncodeInteger (-50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:57" ["100"] (DocTest.asPrint( zzEncodeInteger (50::Integer) )),  DocTest.test "src/Data/ZigZag.hs:60" ["511"] (DocTest.asPrint( zzEncodeInteger (-256::Integer) )),  DocTest.test "src/Data/ZigZag.hs:63" ["512"] (DocTest.asPrint( zzEncodeInteger (256::Integer) ))]
