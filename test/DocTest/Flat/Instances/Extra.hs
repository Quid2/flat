
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Extra where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Extra

tests :: IO TestTree
tests = testGroup "Flat.Instances.Extra" <$> sequence [  DocTest.test "src/Flat/Instances/Extra.hs:10" "[ExpectedLine [LineChunk \"(True,1,\\\"0\\\")\"]]" (DocTest.asPrint( test "" )),  DocTest.test "src/Flat/Instances/Extra.hs:13" "[ExpectedLine [LineChunk \"(True,28,\\\"10110000 11011000 01101100 0010\\\")\"]]" (DocTest.asPrint( test "aaa" ))]
