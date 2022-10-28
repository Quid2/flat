module Main where
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified DocTest.Flat.AsBin

main = (testGroup "DocTests" <$> sequence [DocTest.Flat.AsBin.tests]) >>= defaultMain
