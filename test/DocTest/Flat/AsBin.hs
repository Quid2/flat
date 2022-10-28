{-# LANGUAGE ScopedTypeVariables#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.AsBin where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.AsBin
import Flat.Instances.Base
import Flat.Instances.Text
import Flat.Decoder.Types
import Flat.Types
import Flat.Run
import Data.Word
import qualified Data.Text as T
import Text.PrettyPrint.HughesPJClass

tests :: IO TestTree
tests = testGroup "Flat.AsBin" <$> sequence [  DocTest.test "src/Flat/AsBin.hs:50" "[ExpectedLine [LineChunk \"Right [AsBin {repr = \\\"\\\\129A\\\", offsetBits = 1},AsBin {repr = \\\"A \\\", offsetBits = 2},AsBin {repr = \\\" \\\\193\\\", offsetBits = 3}]\"]]" (DocTest.asPrint( unflat (flat [1::Int .. 3]) :: Decoded ([AsBin Int]) )),  DocTest.test "src/Flat/AsBin.hs:55" "[ExpectedLine [LineChunk \"Right 'a'\"]]" (DocTest.asPrint( unbin <$> (unflat (flat 'a') :: Decoded (AsBin Char)) )),  DocTest.test "src/Flat/AsBin.hs:60" "[ExpectedLine [LineChunk \"3\"]]" (DocTest.asPrint( let Right l :: Decoded [AsBin Int] = unflat (flat [1..5]) in unbin (l  !! 2) )),  DocTest.test "src/Flat/AsBin.hs:65" "[ExpectedLine [LineChunk \"\\\"(0, _0000001 1, _1)\\\"\"]]" (DocTest.asPrint( let Right t :: Decoded (AsBin Bool,AsBin Word8,AsBin Bool) = unflat (flat (False,3:: Word64,True)) in prettyShow t )),  DocTest.test "src/Flat/AsBin.hs:80" "[ExpectedLine [LineChunk \"Right (AsBin {repr = \\\"\\\", offsetBits = 0})\"]]" (DocTest.asPrint( unflat (flat ()) :: Decoded (AsBin ()) )),  DocTest.test "src/Flat/AsBin.hs:83" "[ExpectedLine [LineChunk \"Right (False,AsBin {repr = \\\"A\\\", offsetBits = 1})\"]]" (DocTest.asPrint( unflat (flat (False,True)) :: Decoded (Bool,AsBin Bool) )),  DocTest.test "src/Flat/AsBin.hs:86" "[ExpectedLine [LineChunk \"Right (False,False,AsBin {repr = \\\"?\\\\193\\\", offsetBits = 2})\"]]" (DocTest.asPrint( unflat (flat (False,False,255 :: Word8)) :: Decoded (Bool,Bool,AsBin Word8) )),  DocTest.test "src/Flat/AsBin.hs:89" "[ExpectedLine [LineChunk \"(False,False,255,True)\"]]" (DocTest.asPrint( let Right (b0,b1,rw,b3) :: Decoded (Bool,Bool,AsBin Word8,Bool) = unflat (flat (False,False,255 :: Word8,True)) in (b0,b1,unbin rw,b3) ))]
