module Main where
import           Test.DocTest
import           System.FilePath.Find
import           Data.List                      ( isSuffixOf )

main :: IO ()
-- main = find always ((extension ==? ".hs") &&? exceptFiles ["Data/Convertible/Base.hs","Data/Convertible/Utils.hs","Data/Convertible/Instances/Num.hs"]) "src" >>= doctest
main = find always ((extension ==? ".hs") &&? exceptFiles []) "src" >>= doctest

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
      -- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
      let excludes = liftOp
                (\fp modules -> not $ any (`isSuffixOf` fp) modules)
      in  filePath `excludes` mdls
