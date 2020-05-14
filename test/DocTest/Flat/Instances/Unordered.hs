
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Unordered where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Unordered
import Flat.Instances.Base()
import Flat.Instances.Test
import Data.Word
import qualified Data.HashMap.Strict
import qualified Data.HashMap.Lazy
import qualified Data.HashSet
test = tstBits

tests :: IO TestTree
tests = testGroup "Flat.Instances.Unordered" <$> sequence [  DocTest.test "src/Flat/Instances/Unordered.hs:19" "[ExpectedLine [LineChunk \"(True,28,\\\"10000000 11000000 10100000 0110\\\")\"]]" (DocTest.asPrint( test (Data.HashSet.fromList [1..3::Word]) )),  DocTest.test "src/Flat/Instances/Unordered.hs:29" "[ExpectedLine [LineChunk \"(True,35,\\\"10000001 00001011 01000001 00001011 000\\\")\"]]" (DocTest.asPrint( test (Data.HashMap.Strict.fromList [(1,11),(2,22)]) )),  DocTest.test "src/Flat/Instances/Unordered.hs:32" "[ExpectedLine [LineChunk \"(True,35,\\\"10000001 00001011 01000001 00001011 000\\\")\"]]" (DocTest.asPrint( test (Data.HashMap.Lazy.fromList [(1,11),(2,22)]) ))]
