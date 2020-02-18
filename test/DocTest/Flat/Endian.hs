
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Endian where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Endian
import Numeric (showHex)

tests :: IO TestTree
tests = testGroup "Flat.Endian" <$> sequence [  DocTest.test "src/Flat/Endian.hs:36" ["True"] (DocTest.asPrint( toBE64 0xF0F1F2F3F4F5F6F7 == if isBigEndian then 0xF0F1F2F3F4F5F6F7 else 0xF7F6F5F4F3F2F1F0 )),  DocTest.test "src/Flat/Endian.hs:49" ["True"] (DocTest.asPrint( toBE32 0xF0F1F2F3 == if isBigEndian then 0xF0F1F2F3 else 0xF3F2F1F0 )),  DocTest.test "src/Flat/Endian.hs:62" ["True"] (DocTest.asPrint( toBE16 0xF0F1 == if isBigEndian then 0xF0F1 else 0xF1F0 ))]
