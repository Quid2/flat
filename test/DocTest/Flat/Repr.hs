{-# LANGUAGE ScopedTypeVariables#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Repr where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Repr
import Flat.Instances.Base
import Flat.Decoder.Types
import Flat.Types
import Flat.Run
import Flat.Class

tests :: IO TestTree
tests = testGroup "Flat.Repr" <$> sequence [  DocTest.test "src/Flat/Repr.hs:26" "[ExpectedLine [LineChunk \"Right [Repr {repr = \\\"\\\\STX\\\\SOH\\\"},Repr {repr = \\\"\\\\EOT\\\\SOH\\\"},Repr {repr = \\\"\\\\ACK\\\\SOH\\\"},Repr {repr = \\\"\\\\b\\\\SOH\\\"},Repr {repr = \\\"\\\\n\\\\SOH\\\"}]\"]]" (DocTest.asPrint( unflat (flat [1::Int .. 5]) :: Decoded ([Repr Int]) )),  DocTest.test "src/Flat/Repr.hs:31" "[ExpectedLine [LineChunk \"3\"]]" (DocTest.asPrint( let Right l = unflat (flat [1..5]) :: Decoded [Repr Int] in unrepr (l  !! 2) )),  DocTest.test "src/Flat/Repr.hs:41" "[ExpectedLine [LineChunk \"Right ('a',SizeOf 28,'z',SizeOf 1)\"]]" (DocTest.asPrint( let v = flat ('a',"abc",'z',True) in unflat v :: Decoded (Char,SizeOf String,Char,SizeOf Bool) ))]
