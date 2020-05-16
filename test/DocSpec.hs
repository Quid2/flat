{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ViewPatterns #-}

module Main where

import           Data.List                      ( isSuffixOf )
import           System.FilePath.Find
import           Test.DocTest
import           System.Environment
import qualified Data.Text                     as T
t = main

-- e.g.: stack test :doc --file-watch --fast --test-arguments="Data.ZigZag Flat.Instances Flat.Instances.Base"
main :: IO ()
main = do
  args  <- getArgs
  -- print args
  files <- if length args > 0
    then return $ map
      ( T.unpack
      . (`T.append` ".hs")
      . ("src/" `T.append`)
      . T.replace "." "/"
      . T.pack
      )
      args
    else find always ((extension ==? ".hs") &&? exceptFiles []) "src"
  -- print files
  runTests runOpts files
  genTests genOpts files

runTests opts files = doctest $ opts ++ files

runOpts = ["--fast", "-XCPP"]

genOpts = runOpts ++ ["-Dghcjs_HOST_OS", "-DETA"]

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
  in  filePath `excludes` mdls
-- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
