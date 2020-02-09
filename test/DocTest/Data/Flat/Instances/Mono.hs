
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.Mono where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.Flat.Instances.Mono
import Data.Flat.Instances.Base()
import Data.Flat.Instances.Test
import Data.Word
import qualified Data.Set
import qualified Data.Map

tests :: IO TestTree
tests = testGroup "Data.Flat.Instances.Mono" <$> sequence [  DocTest.test "src/Data/Flat/Instances/Mono.hs:69" ["(True,8,[0])"] (DocTest.asPrint( tst $ AsArray ([]::[()]) )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:72" ["(True,40,[3,11,22,33,0])"] (DocTest.asPrint( tst $ AsArray [11::Word8,22,33] )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:75" ["(True,1,[0])"] (DocTest.asPrint( tst $ AsList ([]::[()]) )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:78" ["(True,28,[133,197,164,32])"] (DocTest.asPrint( tst (AsList [11::Word8,22,33]) )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:81" ["(True,28,[133,197,164,32])"] (DocTest.asPrint( tst (AsSet (Data.Set.fromList [11::Word8,22,33])) )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:170" ["(True,1,[0])"] (DocTest.asPrint( tst (AsMap (Data.Map.fromList ([]::[(Word8,())]))) )),  DocTest.test "src/Data/Flat/Instances/Mono.hs:173" ["(True,18,[129,132,128])"] (DocTest.asPrint( tst (AsMap (Data.Map.fromList [(3::Word,9::Word)])) ))]
