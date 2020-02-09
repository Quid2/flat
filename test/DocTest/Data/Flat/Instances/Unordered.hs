
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.Unordered where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.Flat.Instances.Unordered
import Data.Flat.Instances.Base()
import Data.Flat.Instances.Test
import Data.Word
import qualified Data.HashMap.Strict
import qualified Data.HashMap.Lazy
import qualified Data.HashSet
test = tstBits

tests :: IO TestTree
tests = testGroup "Data.Flat.Instances.Unordered" <$> sequence [  DocTest.test "src/Data/Flat/Instances/Unordered.hs:19" ["(True,28,\"10000000 11000000 10100000 0110\")"] (DocTest.asPrint( test (Data.HashSet.fromList [1..3::Word]) )),  DocTest.test "src/Data/Flat/Instances/Unordered.hs:29" ["(True,35,\"10000001 00001011 01000001 00001011 000\")"] (DocTest.asPrint( test (Data.HashMap.Strict.fromList [(1,11),(2,22)]) )),  DocTest.test "src/Data/Flat/Instances/Unordered.hs:32" ["(True,35,\"10000001 00001011 01000001 00001011 000\")"] (DocTest.asPrint( test (Data.HashMap.Lazy.fromList [(1,11),(2,22)]) ))]
