
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Mono where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Mono
import Flat.Instances.Base()
import Flat.Instances.Test
import Data.Word
import qualified Data.Set
import qualified Data.Map

tests :: IO TestTree
tests = testGroup "Flat.Instances.Mono" <$> sequence [  DocTest.test "src/Flat/Instances/Mono.hs:60" "[ExpectedLine [LineChunk \"(True,8,[0])\"]]" (DocTest.asPrint( tst $ AsArray ([]::[()]) )),  DocTest.test "src/Flat/Instances/Mono.hs:63" "[ExpectedLine [LineChunk \"(True,40,[3,11,22,33,0])\"]]" (DocTest.asPrint( tst $ AsArray [11::Word8,22,33] )),  DocTest.test "src/Flat/Instances/Mono.hs:66" "[ExpectedLine [LineChunk \"(True,1,[0])\"]]" (DocTest.asPrint( tst $ AsList ([]::[()]) )),  DocTest.test "src/Flat/Instances/Mono.hs:69" "[ExpectedLine [LineChunk \"(True,28,[133,197,164,32])\"]]" (DocTest.asPrint( tst (AsList [11::Word8,22,33]) )),  DocTest.test "src/Flat/Instances/Mono.hs:72" "[ExpectedLine [LineChunk \"(True,28,[133,197,164,32])\"]]" (DocTest.asPrint( tst (AsSet (Data.Set.fromList [11::Word8,22,33])) )),  DocTest.test "src/Flat/Instances/Mono.hs:162" "[ExpectedLine [LineChunk \"(True,1,[0])\"]]" (DocTest.asPrint( tst (AsMap (Data.Map.fromList ([]::[(Word8,())]))) )),  DocTest.test "src/Flat/Instances/Mono.hs:165" "[ExpectedLine [LineChunk \"(True,18,[129,132,128])\"]]" (DocTest.asPrint( tst (AsMap (Data.Map.fromList [(3::Word,9::Word)])) ))]
