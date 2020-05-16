
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Bits where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Bits
import Data.Word
import Flat.Instances.Base
import Flat.Instances.Test

tests :: IO TestTree
tests = testGroup "Flat.Bits" <$> sequence [  DocTest.test "src/Flat/Bits.hs:43" "[ExpectedLine [LineChunk \"[True]\"]]" (DocTest.asPrint( bits True )),  DocTest.test "src/Flat/Bits.hs:52" "[ExpectedLine [LineChunk \"[True,False,False,False,False,False,False,True]\"]]" (DocTest.asPrint( paddedBits True )),  DocTest.test "src/Flat/Bits.hs:66" "[ExpectedLine [LineChunk \"[False,False,False,False,False,True,False,True]\"]]" (DocTest.asPrint( asBits (5::Word8) )),  DocTest.test "src/Flat/Bits.hs:72" "[ExpectedLine [LineChunk \"[1,3]\"]]" (DocTest.asPrint( asBytes $ asBits (256+3::Word16) )),  DocTest.test "src/Flat/Bits.hs:87" "[ExpectedLine [LineChunk \"\\\"00000001 00000011\\\"\"]]" (DocTest.asPrint( prettyShow $ asBits (256+3::Word16) ))]
