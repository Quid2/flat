{-# LANGUAGE FlexibleContexts#-}

{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Flat.Instances.Array where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Flat.Instances.Array
import           Flat.Instances.Test
import           Flat.Instances.Mono
import           qualified Data.Array as A
import           qualified Data.Array.Unboxed as U
import           Data.Array.IArray
import           Data.Word

tests :: IO TestTree
tests = testGroup "Flat.Instances.Array" <$> sequence [  DocTest.test "src/Flat/Instances/Array.hs:30" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( let arr = A.array ((1::Word,4::Word),(2,5)) [((1,4),11::Word),((1,5),22),((2,4),33),((2,5),44)] in tst (bounds arr,AsArray(elems arr)) == tst arr )),  DocTest.test "src/Flat/Instances/Array.hs:35" "[ExpectedLine [LineChunk \"(True,80,[1,4,2,5,4,11,22,33,44,0])\"]]" (DocTest.asPrint( tst $ A.array ((1::Word,4::Word),(2,5)) [((1,4),11::Word),((1,5),22),((2,4),33),((2,5),44)] )),  DocTest.test "src/Flat/Instances/Array.hs:38" "[ExpectedLine [LineChunk \"(True,160,[2,8,4,10,4,152,203,166,137,140,186,106,153,75,166,137,148,186,106,0])\"]]" (DocTest.asPrint( tst $ A.array ((1,4),(2,5)) [((1,4),"1.4"),((1,5),"1.5"),((2,4),"2.4"),((2,5),"2.5")] )),  DocTest.test "src/Flat/Instances/Array.hs:43" "[ExpectedLine [LineChunk \"True\"]]" (DocTest.asPrint( let bounds = ((1::Word,4::Word),(2,5));elems=[11::Word,22,33,44] in tst (U.listArray bounds elems :: U.UArray (Word,Word) Word) == tst (A.listArray bounds elems) ))]
