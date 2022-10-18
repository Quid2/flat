
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Text where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Text
import Flat.Instances.Base()
import Flat.Instances.Test
import qualified Data.Text             as T
import qualified Data.Text.Lazy             as TL
import Data.Word
tt t = let (ts,_,bs) = tst t in (ts,bs)

tests :: IO TestTree
tests = testGroup "Flat.Instances.Text" <$> sequence [  DocTest.test "src/Flat/Instances/Text.hs:26" "[ExpectedLine [LineChunk \"(True,[1,0])\"]]" (DocTest.asPrint( tt $ T.pack "" )),  DocTest.test "src/Flat/Instances/Text.hs:29" "[ExpectedLine [LineChunk \"(True,[1,3,97,97,97,0])\"]]" (DocTest.asPrint( tt $ T.pack "aaa" )),  DocTest.test "src/Flat/Instances/Text.hs:32" "[ExpectedLine [LineChunk \"(True,[1,6,194,162,194,162,194,162,0])\"]]" (DocTest.asPrint( tt $ T.pack "¢¢¢" )),  DocTest.test "src/Flat/Instances/Text.hs:35" "[ExpectedLine [LineChunk \"(True,[1,9,230,151,165,230,151,165,230,151,165,0])\"]]" (DocTest.asPrint( tt $ T.pack "日日日" )),  DocTest.test "src/Flat/Instances/Text.hs:39" "[ExpectedLine [LineChunk \"(True,[1,12,240,144,141,136,240,144,141,136,240,144,141,136,0])\"]]" (DocTest.asPrint( tt $ T.pack "𐍈𐍈𐍈" )),  DocTest.test "src/Flat/Instances/Text.hs:45" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( tst (T.pack "abc") == tst (TL.pack "abc") )),  DocTest.test "src/Flat/Instances/Text.hs:63" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( tst (UTF8Text $ T.pack "日日日") == tst (T.pack "日日日") )),  DocTest.test "src/Flat/Instances/Text.hs:76" "[ExpectedLine [LineChunk \"(True,[1,6,97,0,97,0,97,0,0])\"]]" (DocTest.asPrint( tt (UTF16Text $ T.pack "aaa") )),  DocTest.test "src/Flat/Instances/Text.hs:79" "[ExpectedLine [LineChunk \"(True,[1,12,0,216,72,223,0,216,72,223,0,216,72,223,0])\"]]" (DocTest.asPrint( tt (UTF16Text $ T.pack "𐍈𐍈𐍈") ))]
