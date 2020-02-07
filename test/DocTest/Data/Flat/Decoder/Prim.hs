{-# LANGUAGE BinaryLiterals#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Decoder.Prim where
import qualified DocTest
import Test.Tasty(testGroup)
import Data.Flat.Decoder.Prim
import Data.Word
import Data.Flat.Run

tests = testGroup "Data.Flat.Decoder.Prim" <$> sequence [  DocTest.test "src/Data/Flat/Decoder/Prim.hs:177" ["True"] (DocTest.asPrint( unflatWith (dBEBits8 3) [0b11100001::Word8] == Right 0b00000111 ))]
