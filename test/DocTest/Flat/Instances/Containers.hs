
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Containers where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Containers
import Flat.Instances.Test
import Data.Set
import Data.Sequence
import Data.IntMap
import Data.Map
import Data.Tree
import Flat.Instances.Mono

tests :: IO TestTree
tests = testGroup "Flat.Instances.Containers" <$> sequence [  DocTest.test "src/Flat/Instances/Containers.hs:44" "[ExpectedLine [LineChunk \"(True,1,[0])\"]]" (DocTest.asPrint( tst (Data.IntMap.empty :: IntMap ()) )),  DocTest.test "src/Flat/Instances/Containers.hs:47" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( asList Data.IntMap.fromList [(1,"a"),(2,"b")] )),  DocTest.test "src/Flat/Instances/Containers.hs:58" "[ExpectedLine [LineChunk \"(True,1,[0])\"]]" (DocTest.asPrint( tst (Data.Map.empty :: Map () ()) )),  DocTest.test "src/Flat/Instances/Containers.hs:61" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( asList Data.Map.fromList [("a","aa"),("b","bb")] )),  DocTest.test "src/Flat/Instances/Containers.hs:66" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( let l = [("a","aa"),("b","bb")] in tst (Data.Map.fromList l) == tst (Data.Map.fromList $ Prelude.reverse l) )),  DocTest.test "src/Flat/Instances/Containers.hs:71" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( let l = [(2::Int,"b"),(1,"a")] in tst (Data.IntMap.fromList l) == tst (Data.Map.fromList l) )),  DocTest.test "src/Flat/Instances/Containers.hs:82" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( asList Data.Sequence.fromList [3::Word8,4,7] )),  DocTest.test "src/Flat/Instances/Containers.hs:93" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( asList Data.Set.fromList [3::Word8,4,7] )),  DocTest.test "src/Flat/Instances/Containers.hs:102" "[ExpectedLine [LineChunk \"(True,39,[1,129,64,200,32])\"]]" (DocTest.asPrint( tst (Node (1::Word8) [Node 2 [Node 3 []], Node 4 []]) ))]
