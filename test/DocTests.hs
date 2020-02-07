module Main where
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified DocTest.Data.Flat.Instances.ByteString
import qualified DocTest.Data.Flat.Instances.DList
import qualified DocTest.Data.Flat.Instances.Containers
import qualified DocTest.Data.Flat.Instances.Base
import qualified DocTest.Data.Flat.Instances.Unordered
import qualified DocTest.Data.Flat.Instances.Vector
import qualified DocTest.Data.Flat.Instances.Mono
import qualified DocTest.Data.Flat.Instances.Text
import qualified DocTest.Data.ZigZag
import qualified DocTest.Data.Flat.Decoder.Prim
import qualified DocTest.Data.FloatCast

main = (testGroup "DocTests" <$> sequence [DocTest.Data.Flat.Instances.ByteString.tests,DocTest.Data.Flat.Instances.DList.tests,DocTest.Data.Flat.Instances.Containers.tests,DocTest.Data.Flat.Instances.Base.tests,DocTest.Data.Flat.Instances.Unordered.tests,DocTest.Data.Flat.Instances.Vector.tests,DocTest.Data.Flat.Instances.Mono.tests,DocTest.Data.Flat.Instances.Text.tests,DocTest.Data.ZigZag.tests,DocTest.Data.Flat.Decoder.Prim.tests,DocTest.Data.FloatCast.tests]) >>= defaultMain
