module Main where
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified DocTest.Flat.Bits
import qualified DocTest.Flat.Repr
import qualified DocTest.Flat.Instances.Array
import qualified DocTest.Flat.Instances.ByteString
import qualified DocTest.Flat.Instances.DList
import qualified DocTest.Flat.Instances.Extra
import qualified DocTest.Flat.Instances.Containers
import qualified DocTest.Flat.Instances.Base
import qualified DocTest.Flat.Instances.Unordered
import qualified DocTest.Flat.Instances.Vector
import qualified DocTest.Flat.Instances.Mono
import qualified DocTest.Flat.Instances.Text
import qualified DocTest.Flat.Encoder.Prim
import qualified DocTest.Flat.Tutorial
import qualified DocTest.Flat.Decoder.Prim
import qualified DocTest.Flat.Endian
import qualified DocTest.Data.ZigZag
import qualified DocTest.Data.FloatCast

main = (testGroup "DocTests" <$> sequence [DocTest.Flat.Bits.tests,DocTest.Flat.Repr.tests,DocTest.Flat.Instances.Array.tests,DocTest.Flat.Instances.ByteString.tests,DocTest.Flat.Instances.DList.tests,DocTest.Flat.Instances.Extra.tests,DocTest.Flat.Instances.Containers.tests,DocTest.Flat.Instances.Base.tests,DocTest.Flat.Instances.Unordered.tests,DocTest.Flat.Instances.Vector.tests,DocTest.Flat.Instances.Mono.tests,DocTest.Flat.Instances.Text.tests,DocTest.Flat.Encoder.Prim.tests,DocTest.Flat.Tutorial.tests,DocTest.Flat.Decoder.Prim.tests,DocTest.Flat.Endian.tests,DocTest.Data.ZigZag.tests,DocTest.Data.FloatCast.tests]) >>= defaultMain
