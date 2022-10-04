
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
tests = testGroup "Flat.Instances.Mono" <$> sequence [  DocTest.test "src/Flat/Instances/Mono.hs:62" "[ExpectedLine [LineChunk \"\\\"1111110\\\"\"]]" (DocTest.asPrint( flatBits $ AsList [True,True,True] )),  DocTest.test "src/Flat/Instances/Mono.hs:69" "[ExpectedLine [LineChunk \"\\\"1111110\\\"\"]]" (DocTest.asPrint( flatBits $ [True,True,True] )),  DocTest.test "src/Flat/Instances/Mono.hs:74" "[ExpectedLine [LineChunk \"\\\"00000011 11100000 000\\\"\"]]" (DocTest.asPrint( flatBits $ AsArray [True,True,True] )),  DocTest.test "src/Flat/Instances/Mono.hs:79" "[ExpectedLine [LineChunk \"\\\"10000001 11110000 00000\\\"\"]]" (DocTest.asPrint( flatBits $ [AsArray [True,True,True]] )),  DocTest.test "src/Flat/Instances/Mono.hs:82" "[ExpectedLine [LineChunk \"\\\"11100000 11111111 11000000 00\\\"\"]]" (DocTest.asPrint( flatBits $ (True,True,True,AsArray $ replicate 7 True) )),  DocTest.test "src/Flat/Instances/Mono.hs:85" "[ExpectedLine [LineChunk \"\\\"00000000\\\"\"]]" (DocTest.asPrint( flatBits $ AsArray ([]::[()]) )),  DocTest.test "src/Flat/Instances/Mono.hs:88" "[ExpectedLine [LineChunk \"\\\"0\\\"\"]]" (DocTest.asPrint( flatBits $ AsList ([]::[()]) )),  DocTest.test "src/Flat/Instances/Mono.hs:91" "[ExpectedLine [LineChunk \"(True,28,[133,197,164,32])\"]]" (DocTest.asPrint( tst (AsList [11::Word8,22,33]) )),  DocTest.test "src/Flat/Instances/Mono.hs:94" "[ExpectedLine [LineChunk \"(True,28,[133,197,164,32])\"]]" (DocTest.asPrint( tst (AsSet (Data.Set.fromList [11::Word8,22,33])) )),  DocTest.test "src/Flat/Instances/Mono.hs:97" "[ExpectedLine [LineChunk \"(True,99,[129,129,2,3,0,65,66,2,131,3,132,0,0])\"]]" (DocTest.asPrint( tst [AsArray [1..3], AsArray [4..8]] )),  DocTest.test "src/Flat/Instances/Mono.hs:100" "[ExpectedLine [LineChunk \"(True,99,[129,128,129,1,128,65,65,1,65,129,194,0,0])\"]]" (DocTest.asPrint( tst $ [AsArray [(1::Word8)..3], AsArray [4..8]] )),  DocTest.test "src/Flat/Instances/Mono.hs:103" "[ExpectedLine [LineChunk \"(True,42,[129,129,2,3,0,0])\"]]" (DocTest.asPrint( tst $ [AsArray [(1::Int)..3]] )),  DocTest.test "src/Flat/Instances/Mono.hs:196" "[ExpectedLine [LineChunk \"(True,1,[0])\"]]" (DocTest.asPrint( tst (AsMap (Data.Map.fromList ([]::[(Word8,())]))) )),  DocTest.test "src/Flat/Instances/Mono.hs:199" "[ExpectedLine [LineChunk \"(True,18,[129,132,128])\"]]" (DocTest.asPrint( tst (AsMap (Data.Map.fromList [(3::Word,9::Word)])) ))]
