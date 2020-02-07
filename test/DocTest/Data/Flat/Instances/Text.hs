
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Data.Flat.Instances.Text where
import qualified DocTest
import Test.Tasty(testGroup)
import Data.Flat.Instances.Text
import Data.Flat.Instances.Base()
import Data.Flat.Instances.Test
import qualified Data.Text             as T
import qualified Data.Text.Lazy             as TL
import Data.Word

tests = testGroup "Data.Flat.Instances.Text" <$> sequence [  DocTest.test "src/Data/Flat/Instances/Text.hs:20" ["(True,16,[1,0])"] (DocTest.asPrint( tst $ T.pack "" )),  DocTest.test "src/Data/Flat/Instances/Text.hs:23" ["(True,120,[1,3,97,97,97,0])"] (DocTest.asPrint( tst $ T.pack "aaa" )),  DocTest.test "src/Data/Flat/Instances/Text.hs:26" ["(True,120,[1,6,194,162,194,162,194,162,0])"] (DocTest.asPrint( tst $ T.pack "¢¢¢" )),  DocTest.test "src/Data/Flat/Instances/Text.hs:29" ["(True,120,[1,9,230,151,165,230,151,165,230,151,165,0])"] (DocTest.asPrint( tst $ T.pack "日日日" )),  DocTest.test "src/Data/Flat/Instances/Text.hs:32" ["(True,120,[1,12,240,144,141,136,240,144,141,136,240,144,141,136,0])"] (DocTest.asPrint( tst $ T.pack "𐍈𐍈𐍈" )),  DocTest.test "src/Data/Flat/Instances/Text.hs:37" ["True"] (DocTest.asPrint( tst (T.pack "𐍈𐍈𐍈") == tst (TL.pack "𐍈𐍈𐍈") )),  DocTest.test "src/Data/Flat/Instances/Text.hs:53" ["True"] (DocTest.asPrint( tst (UTF8Text $ T.pack "日日日") == tst (T.pack "日日日") )),  DocTest.test "src/Data/Flat/Instances/Text.hs:56" ["(True,72,[1,6,97,0,97,0,97,0,0])"] (DocTest.asPrint( tst (UTF16Text $ T.pack "aaa") )),  DocTest.test "src/Data/Flat/Instances/Text.hs:59" ["(True,120,[1,12,0,216,72,223,0,216,72,223,0,216,72,223,0])"] (DocTest.asPrint( tst (UTF16Text $ T.pack "𐍈𐍈𐍈") ))]
