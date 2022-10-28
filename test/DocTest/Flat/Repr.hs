{-# LANGUAGE ScopedTypeVariables,TypeApplications#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Repr where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Repr
import Flat.Instances.Base
import Flat.Instances.Text
import Flat.Decoder.Types
import Flat.Types
import Flat.Run
import Data.Word
import qualified Data.Text as T

tests :: IO TestTree
tests = testGroup "Flat.Repr" <$> sequence [  DocTest.test "src/Flat/Repr.hs:35" "[ExpectedLine [LineChunk \"Right [Repr {repr = \\\"\\\\129A\\\", offsetBits = 1},Repr {repr = \\\"A \\\", offsetBits = 2},Repr {repr = \\\" \\\\208\\\", offsetBits = 3},Repr {repr = \\\"\\\\208\\\\136\\\", offsetBits = 4},Repr {repr = \\\"\\\\136Q\\\", offsetBits = 5}]\"]]" (DocTest.asPrint( unflat (flat [1::Int .. 5]) :: Decoded ([Repr Int]) )),  DocTest.test "src/Flat/Repr.hs:40" "[ExpectedLine [LineChunk \"3\"]]" (DocTest.asPrint( let Right l = unflat (flat [1..5]) :: Decoded [Repr Int] in unrepr (l  !! 2) )),  DocTest.test "src/Flat/Repr.hs:43" "[ExpectedLine [LineChunk \"Right (Repr {repr = \\\"\\\", offsetBits = 0})\"]]" (DocTest.asPrint( unflat @(Repr ()) (flat ()) )),  DocTest.test "src/Flat/Repr.hs:46" "[ExpectedLine [LineChunk \"Right (False,Repr {repr = \\\"A\\\", offsetBits = 1})\"]]" (DocTest.asPrint( unflat @(Bool,Repr Bool) (flat (False,True)) )),  DocTest.test "src/Flat/Repr.hs:49" "[ExpectedLine [LineChunk \"Right (False,False,Repr {repr = \\\"?\\\\193\\\", offsetBits = 2})\"]]" (DocTest.asPrint( unflat @(Bool,Bool,Repr Word8) (flat (False,False,255 :: Word8)) )),  DocTest.test "src/Flat/Repr.hs:52" "[ExpectedLine [LineChunk \"(False,False,255,True)\"]]" (DocTest.asPrint( let Right (b0,b1,rw,b3) = unflat @(Bool,Bool,Repr Word8,Bool) (flat (False,False,255 :: Word8,True)) in (b0,b1,unrepr rw,b3) )),  DocTest.test "src/Flat/Repr.hs:55" "[ExpectedLine [LineChunk \"Right 'a'\"]]" (DocTest.asPrint( unrepr <$> unflat @(Repr Char) (flat 'a') )),  DocTest.test "src/Flat/Repr.hs:58" "[ExpectedLine [LineChunk \"Right \\\"(False, False, __111111 11)\\\"\"]]" (DocTest.asPrint( prettyShow <$> unflat @(Bool,Bool,Repr Word8) (flat (False,False,255 :: Word8)) )),  DocTest.test "src/Flat/Repr.hs:68" "[ExpectedLine [LineChunk \"Right (SizeOf 8,SizeOf 28,'z',SizeOf 1)\"]]" (DocTest.asPrint( let v = flat ('a',"abc",'z',True) in unflat v :: Decoded (SizeOf Char,SizeOf String,Char,SizeOf Bool) )),  DocTest.test "src/Flat/Repr.hs:75" "[ExpectedLine [LineChunk \"Right (SizeOf 16,SizeOf 1,[SizeOf 14,SizeOf 31,SizeOf 47],SizeOf 8,SizeOf 24)\"]]" (DocTest.asPrint( unflat @ (SizeOf T.Text,SizeOf Bool,[SizeOf T.Text],SizeOf Word16,SizeOf Word32) $ flat (T.pack "",False,[T.pack "",T.pack "a",T.pack "主"],1::Word16,66000::Word32) ))]
