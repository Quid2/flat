
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Extra where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Extra
import Flat.Instances.Test

tests :: IO TestTree
tests = testGroup "Flat.Instances.Extra" <$> sequence [  DocTest.test "src/Flat/Instances/Extra.hs:13" "[ExpectedLine [LineChunk \"(True,1,\\\"0\\\")\"]]" (DocTest.asPrint( tstBits "" )),  DocTest.test "src/Flat/Instances/Extra.hs:16" "[ExpectedLine [LineChunk \"(True,28,\\\"10110000 11011000 01101100 0010\\\")\"]]" (DocTest.asPrint( tstBits "aaa" ))]
