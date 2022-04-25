{-# LANGUAGE BinaryLiterals#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Decoder.Prim where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Decoder.Prim
import Data.Word
import Data.Int
import Flat.Run

tests :: IO TestTree
tests = testGroup "Flat.Decoder.Prim" <$> sequence [  DocTest.test "src/Flat/Decoder/Prim.hs:186" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( unflatWith (dBEBits8 3) [0b11100001::Word8] == Right 0b00000111 ))]
