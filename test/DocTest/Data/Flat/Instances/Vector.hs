
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.Vector where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.Flat.Instances.Vector
import Data.Flat.Instances.Test
import Data.Flat.Instances.Base()
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Storable          as S

tests :: IO TestTree
tests = testGroup "Data.Flat.Instances.Vector" <$> sequence [  DocTest.test "src/Data/Flat/Instances/Vector.hs:21" ["(True,40,[3,11,22,33,0])"] (DocTest.asPrint( tst (V.fromList [11::Word8,22,33]) )),  DocTest.test "src/Data/Flat/Instances/Vector.hs:26" ["True"] (DocTest.asPrint( let l = [11::Word8,22,33] in all (tst (V.fromList l) ==) [tst (U.fromList l),tst (S.fromList l)] ))]