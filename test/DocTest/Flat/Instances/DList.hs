
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.DList where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.DList
import Flat.Instances.Test
import Flat.Instances.Base()
import Data.DList
test = tstBits

tests :: IO TestTree
tests = testGroup "Flat.Instances.DList" <$> sequence [  DocTest.test "src/Flat/Instances/DList.hs:16" "[ExpectedLine [LineChunk \"(True,19,\\\"10000011 11000001 110\\\")\"]]" (DocTest.asPrint( test (Data.DList.fromList [7::Word,7]) ))]
