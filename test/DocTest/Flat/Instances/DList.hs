
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.DList where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.DList
import Flat.Instances.Test
import Flat.Instances.Base()
import Flat.Run
import Data.DList
test = tstBits

tests :: IO TestTree
tests = testGroup "Flat.Instances.DList" <$> sequence [  DocTest.test "src/Flat/Instances/DList.hs:17" "[ExpectedLine [LineChunk \"(True,19,\\\"10000011 11000001 110\\\")\"]]" (DocTest.asPrint( test (Data.DList.fromList [7::Word,7]) )),  DocTest.test "src/Flat/Instances/DList.hs:20" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( let l = [7::Word,7] in flat (Data.DList.fromList l) == flat l ))]
