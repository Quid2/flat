{-# LANGUAGE DeriveGeneric,DeriveAnyClass#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Tutorial where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Tutorial
import Flat
import Flat.Instances.Test (flatBits,allBits)
data Result = Bad | Good deriving (Show,Generic,Flat)
data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)

tests :: IO TestTree
tests = testGroup "Flat.Tutorial" <$> sequence [  DocTest.test "src/Flat/Tutorial.hs:41" "[ExpectedLine [LineChunk \"\\\"\\\\149\\\"\"]]" (DocTest.asPrint( flat $ Cons North (Cons South Nil) )),  DocTest.test "src/Flat/Tutorial.hs:48" "[ExpectedLine [LineChunk \"Right (Cons North (Cons South Nil))\"]]" (DocTest.asPrint( unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction) )),  DocTest.test "src/Flat/Tutorial.hs:61" "[ExpectedLine [LineChunk \"\\\"1\\\"\"]]" (DocTest.asPrint( flatBits Good )),  DocTest.test "src/Flat/Tutorial.hs:64" "[ExpectedLine [LineChunk \"\\\"0\\\"\"]]" (DocTest.asPrint( flatBits (Nil::List Direction) )),  DocTest.test "src/Flat/Tutorial.hs:69" "[ExpectedLine [LineChunk \"\\\"01\\\"\"]]" (DocTest.asPrint( flatBits South )),  DocTest.test "src/Flat/Tutorial.hs:72" "[ExpectedLine [LineChunk \"\\\"111\\\"\"]]" (DocTest.asPrint( flatBits West )),  DocTest.test "src/Flat/Tutorial.hs:85" "[ExpectedLine [LineChunk \"\\\"1001010\\\"\"]]" (DocTest.asPrint( flatBits $ Cons North (Cons South Nil) )),  DocTest.test "src/Flat/Tutorial.hs:90" "[ExpectedLine [LineChunk \"\\\"10010101\\\"\"]]" (DocTest.asPrint( allBits $ Cons North (Cons South Nil) ))]
