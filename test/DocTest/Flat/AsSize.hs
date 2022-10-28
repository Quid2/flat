{-# LANGUAGE ScopedTypeVariables#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.AsSize where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.AsSize
import Flat.Instances.Base
import Flat.Instances.Text
import Flat.Decoder.Types
import Flat.Types
import Flat.Run
import Data.Word
import qualified Data.Text as T

tests :: IO TestTree
tests = testGroup "Flat.AsSize" <$> sequence [  DocTest.test "src/Flat/AsSize.hs:38" "[ExpectedLine [LineChunk \"Right ('a',AsSize 28,'z',AsSize 1)\"]]" (DocTest.asPrint( let v = flat ('a',"abc",'z',True) in unflat v :: Decoded (Char,AsSize String,Char,AsSize Bool) )),  DocTest.test "src/Flat/AsSize.hs:43" "[ExpectedLine [LineChunk \"Right (AsSize 8,AsSize 8)\"]]" (DocTest.asPrint( unflat (flat (1::Word16,1::Word64)) :: Decoded (AsSize Word16,AsSize Word64) )),  DocTest.test "src/Flat/AsSize.hs:48" "[ExpectedLine [LineChunk \"Right (AsSize 16,AsSize 32,AsSize 48,AsSize 48,AsSize 40,AsSize 40)\"]]" (DocTest.asPrint( unflat (flat (T.pack "",T.pack "a",T.pack "主",UTF8Text $ T.pack "主",UTF16Text $ T.pack "主",UTF16Text $ T.pack "a")) :: Decoded (AsSize T.Text,AsSize T.Text,AsSize T.Text,AsSize UTF8Text,AsSize UTF16Text,AsSize UTF16Text) )),  DocTest.test "src/Flat/AsSize.hs:53" "[ExpectedLine [LineChunk \"Right (AsSize 1,AsSize 96,AsSize 8)\"]]" (DocTest.asPrint( unflat (flat (False,[T.pack "",T.pack "a",T.pack "主"],'a')) :: Decoded (AsSize Bool,AsSize [T.Text],AsSize Char) ))]
