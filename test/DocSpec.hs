module Main where
import Test.DocTest
import System.FilePath.Find

main :: IO ()
-- main = find always ((extension ==? ".hs") ||? (extension ==? ".lhs")) "src" >>= doctest
main = find always (extension ==? ".hs") "src" >>= doctest
