
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Encoder.Prim where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Encoder.Prim
import Flat.Instances.Test
import Flat.Bits
import Flat.Encoder.Strict
import Control.Monad
enc e = prettyShow $ encBits 256 (Encoding e)

tests :: IO TestTree
tests = testGroup "Flat.Encoder.Prim" <$> sequence [  DocTest.test "src/Flat/Encoder/Prim.hs:356" "[ExpectedLine [LineChunk \"\\\"1\\\"\"]]" (DocTest.asPrint( enc eTrueF )),  DocTest.test "src/Flat/Encoder/Prim.hs:364" "[ExpectedLine [LineChunk \"\\\"0\\\"\"]]" (DocTest.asPrint( enc eFalseF )),  DocTest.test "src/Flat/Encoder/Prim.hs:374" "[ExpectedLine [LineChunk \"\\\"10000001\\\"\"]]" (DocTest.asPrint( enc $ eTrueF >=> eFillerF )),  DocTest.test "src/Flat/Encoder/Prim.hs:377" "[ExpectedLine [LineChunk \"\\\"00000001\\\"\"]]" (DocTest.asPrint( enc eFillerF )),  DocTest.test "src/Flat/Encoder/Prim.hs:410" "[ExpectedLine [LineChunk \"\\\"11111111\\\"\"]]" (DocTest.asPrint( enc $ \s-> eWord8F 0 s >>= writeWord8 255 s )),  DocTest.test "src/Flat/Encoder/Prim.hs:413" "[ExpectedLine [LineChunk \"\\\"10000000 01111111 1\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eTrueF s0 >>= \s1 -> eWord8F 255 s1 >>= eWord8F 255 >>= writeWord8 0 s1 )),  DocTest.test "src/Flat/Encoder/Prim.hs:416" "[ExpectedLine [LineChunk \"\\\"01111111 1\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eFalseF s0 >>= \s1 -> eWord8F 0 s1 >>= writeWord8 255 s1 )),  DocTest.test "src/Flat/Encoder/Prim.hs:419" "[ExpectedLine [LineChunk \"\\\"01111111 10\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eFalseF s0 >>= \s1 -> eWord8F 0 s1 >>= writeWord8 255 s1 >>= eFalseF )),  DocTest.test "src/Flat/Encoder/Prim.hs:422" "[ExpectedLine [LineChunk \"\\\"10000000 011\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eTrueF s0 >>= \s1 -> eWord8F 255 s1 >>= eTrueF >>= writeWord8 0 s1 >>= eTrueF ))]
