
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.DList where
import qualified DocTest
import Test.Tasty(testGroup)
import Data.Flat.Instances.DList
import Data.Flat.Instances.Test
import Data.Flat.Instances.Base()
import Data.DList
test = tstBits

tests = testGroup "Data.Flat.Instances.DList" <$> sequence [  DocTest.test "src/Data/Flat/Instances/DList.hs:14" ["(True,19,\"10000011 11000001 110\")"] (DocTest.asPrint( test (Data.DList.fromList [7::Word,7]) ))]
