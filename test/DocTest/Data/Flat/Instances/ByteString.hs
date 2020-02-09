
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.ByteString where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Data.Flat.Instances.ByteString
import Data.Flat.Instances.Test
import Data.Flat.Instances.Base
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Short         as SBS

tests :: IO TestTree
tests = testGroup "Data.Flat.Instances.ByteString" <$> sequence [  DocTest.test "src/Data/Flat/Instances/ByteString.hs:37" ["(True,48,[1,3,11,22,33,0])"] (DocTest.asPrint( tst (B.pack [11,22,33]) )),  DocTest.test "src/Data/Flat/Instances/ByteString.hs:50" ["(True,16,[1,0])"] (DocTest.asPrint( tst (B.pack []) )),  DocTest.test "src/Data/Flat/Instances/ByteString.hs:55" ["(True,51,[65,3,11,22,33,0])"] (DocTest.asPrint( tst ((False,True,False,B.pack [11,22,33])) )),  DocTest.test "src/Data/Flat/Instances/ByteString.hs:60" ["True"] (DocTest.asPrint( all (tst (B.pack [55]) ==) [tst (L.pack [55]),tst (SBS.pack [55])] )),  DocTest.test "src/Data/Flat/Instances/ByteString.hs:69" ["(True,51,[65,3,11,22,33,0])"] (DocTest.asPrint( tst ((False,True,False,L.pack [11,22,33])) )),  DocTest.test "src/Data/Flat/Instances/ByteString.hs:78" ["(True,51,[65,3,11,22,33,0])"] (DocTest.asPrint( tst ((False,True,False,SBS.pack [11,22,33])) ))]
