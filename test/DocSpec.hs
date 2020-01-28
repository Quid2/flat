{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                      ( isSuffixOf )
-- import           System.FilePath
import           System.FilePath.Find
import           Test.DocTest
import           System.Environment
import qualified Data.Text                     as T

main :: IO ()
main = do
  args <- getArgs
  print args
  files <- if length args > 0
    then return $ map
      (T.unpack . (`T.append` ".hs") . ("src/" `T.append`) . T.replace "." "/" . T.pack)
      args
    else find always ((extension ==? ".hs") &&? exceptFiles []) "src"
  print files
  doctest $ "-XCPP" : files

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
  in  filePath `excludes` mdls
-- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
