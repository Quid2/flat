{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Data.List            (isSuffixOf)
import qualified Data.Text            as T
import           System.Environment
import           System.FilePath.Find
import           Test.DocTest         (doctest, genTests)
t = main

-- e.g.: stack test :doc --file-watch --fast --test-arguments="Data.ZigZag Flat.Instances Flat.Instances.Base"
main :: IO ()
main = do
  args  <- getArgs
  -- print args
  files <- if not (null args)
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

-- static tests are generated with ghcjs compatibility as they cannot be generated in ghcjs
genOpts = runOpts ++ ["-Dghcjs_HOST_OS", "-DETA"]

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
  in  filePath `excludes` mdls
-- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
