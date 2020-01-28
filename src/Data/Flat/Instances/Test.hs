-- | doctest utilities
module Data.Flat.Instances.Test(tst,tstBits,asList,module Data.Word) where

import Data.Flat.Class(Flat(..))
import Data.Flat.Run(flat,unflat)
import Data.Flat.Bits(bits,asBytes)
import Data.Flat.Types(NumBits)
import Data.Word
import Text.PrettyPrint.HughesPJClass(prettyShow)

-- |Returns: result of flat/unflat test, encoding size in bits, byte encoding
tst :: (Eq a, Flat a) => a -> (Bool, NumBits, [Word8])
tst v = (unflat (flat v) == Right v,size v 0,asBytes . bits $ v) 

-- |Returns: result of flat/unflat test, encoding size in bits, bits encoding
tstBits :: (Eq a, Flat a) => a -> (Bool, NumBits, String)
tstBits v = (unflat (flat v) == Right v,Data.Flat.Class.size v 0,prettyShow . bits $ v) 

-- |Test that container is serialised as a List
asList :: (Eq a1, Eq a2, Flat a1, Flat a2) => (a2 -> a1) -> a2 -> Bool
asList f l = tst (f l) == tst l
