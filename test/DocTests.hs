module Main where
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified DocTest.Flat.Instances.Containers

main = (testGroup "DocTests" <$> sequence [DocTest.Flat.Instances.Containers.tests]) >>= defaultMain
