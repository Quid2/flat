module Test.Data2.Flat(module Test.Data2) where
import Data.Flat
import Test.Data2

instance Flat a => Flat (List a)
