{-# LANGUAGE BinaryLiterals#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Decoder.Prim where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Decoder.Prim
import Data.Word
import Data.Int
import Flat.Run
import Flat.Bits
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

tests :: IO TestTree
tests = testGroup "Flat.Decoder.Prim" <$> sequence [  DocTest.test "src/Flat/Decoder/Prim.hs:198" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( unflatWith (dBEBits8 3) [0b11100001::Word8] == Right 0b00000111 )),  DocTest.test "src/Flat/Decoder/Prim.hs:201" "[ExpectedLine [LineChunk \"Left (BadOp \\\"read8: cannot read 9 bits\\\")\"]]" (DocTest.asPrint( unflatWith (dBEBits8 9) [0b11100001::Word8,0b11111111] )),  DocTest.test "src/Flat/Decoder/Prim.hs:214" "[ExpectedLine [LineChunk \"Right 00000101 10111111\"]]" (DocTest.asPrint( pPrint . asBits <$> unflatWith (dBEBits16 11) [0b10110111::Word8,0b11100001] )),  DocTest.test "src/Flat/Decoder/Prim.hs:219" "[ExpectedLine [LineChunk \"Right 00000111 11111111\"]]" (DocTest.asPrint( pPrint . asBits <$> unflatWith (dBEBits16 19) [0b00000000::Word8,0b11111111,0b11100001] ))]
