
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Encoder.Prim where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Encoder.Prim
import Flat.Instances.Test
import Flat.Bits
import           Flat.Encoder.Strict
enc e = prettyShow $ encBits 256 (Encoding e)

tests :: IO TestTree
tests = testGroup "Flat.Encoder.Prim" <$> sequence [  DocTest.test "src/Flat/Encoder/Prim.hs:355" "[ExpectedLine [LineChunk \"\\\"1\\\"\"]]" (DocTest.asPrint( enc eTrueF )),  DocTest.test "src/Flat/Encoder/Prim.hs:363" "[ExpectedLine [LineChunk \"\\\"0\\\"\"]]" (DocTest.asPrint( enc eFalseF )),  DocTest.test "src/Flat/Encoder/Prim.hs:373" "[ExpectedLine [LineChunk \"\\\"10000001\\\"\"]]" (DocTest.asPrint( enc $ eTrueF >=> eFillerF )),  DocTest.test "src/Flat/Encoder/Prim.hs:376" "[ExpectedLine [LineChunk \"\\\"00000001\\\"\"]]" (DocTest.asPrint( enc eFillerF )),  DocTest.test "src/Flat/Encoder/Prim.hs:390" "[ExpectedLine [LineChunk \"\\\"00111111 11\\\"\"]]" (DocTest.asPrint( enc (eFalseF >=> eFalseF >=> eByteUnaligned 255) )),  DocTest.test "src/Flat/Encoder/Prim.hs:401" "[ExpectedLine [LineChunk \"\\\"11111111\\\"\"]]" (DocTest.asPrint( enc (eFalseF >=> eFalseF >=> eFalseF >=> eByteAligned 255) )),  DocTest.test "src/Flat/Encoder/Prim.hs:409" "[ExpectedLine [LineChunk \"\\\"11111111\\\"\"]]" (DocTest.asPrint( enc $ \s-> eWord8F 0 s >>= writeWord8 255 s )),  DocTest.test "src/Flat/Encoder/Prim.hs:412" "[ExpectedLine [LineChunk \"\\\"10000000 01111111 1\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eTrueF s0 >>= \s1 -> eWord8F 255 s1 >>= eWord8F 255 >>= writeWord8 0 s1 )),  DocTest.test "src/Flat/Encoder/Prim.hs:415" "[ExpectedLine [LineChunk \"\\\"01111111 1\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eFalseF s0 >>= \s1 -> eWord8F 0 s1 >>= writeWord8 255 s1 )),  DocTest.test "src/Flat/Encoder/Prim.hs:418" "[ExpectedLine [LineChunk \"\\\"01111111 10\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eFalseF s0 >>= \s1 -> eWord8F 0 s1 >>= writeWord8 255 s1 >>= eFalseF )),  DocTest.test "src/Flat/Encoder/Prim.hs:421" "[ExpectedLine [LineChunk \"\\\"10000000 011\\\"\"]]" (DocTest.asPrint( enc $ \s0 -> eTrueF s0 >>= \s1 -> eWord8F 255 s1 >>= eTrueF >>= writeWord8 0 s1 >>= eTrueF ))]
