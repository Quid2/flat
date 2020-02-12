{-# LANGUAGE OverloadedStrings ,NoMonomorphismRestriction ,FlexibleContexts ,ViewPatterns #-}

module Main where

import           Data.List                      ( isSuffixOf )
import           System.FilePath
import           System.FilePath.Find
import           System.Directory
import           Test.DocTest
import           System.Environment
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.Megaparsec -- (Parser,parseMaybe)
import           Text.Megaparsec.Char           ( char
                                                , string
                                                , space
                                                , letterChar
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void
import           Data.Either

t = main
-- e.g.: stack test :doc --file-watch --fast --test-arguments="Data.ZigZag Data.Flat.Instances Data.Flat.Instances.Base"
main :: IO ()
main = do
  args <- getArgs
  print args
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
  print files
  runTests runOpts files
  genTests genOpts files

runTests opts files = doctest $ opts ++ files

runOpts = ["--fast", "-XCPP"]
getOpts = runOpts ++ ["-Dghcjs_HOST_OS", "-DETA"]
 -- "--verbose", 

genTests opts files = do
  mdls <- doctest2 $ opts ++ files -- "-DETA"
  print mdls
  testAll mdls
  mapM_ testFile mdls

testAll mdls =
  let names = map (\mdl -> "DocTest." <> T.pack (moduleName mdl)) mdls
  in  writeModule "test/DocTests.hs"
        . T.unlines
        $ [ "module Main where"
          , "import           Test.Tasty"
          , "import           Test.Tasty.HUnit"
          , T.unlines $ map ("import qualified " <>) names
          , "main = (testGroup \"DocTests\" <$> sequence ["
          <> (T.intercalate "," $ map (<> ".tests") names)
          <> "]) >>= defaultMain"
          ]

testFile mdl = do
  let
    mdlNameS    = moduleName mdl
    mdlName     = T.pack mdlNameS
    path = T.unpack $ "test/DocTest/" <> T.replace "." "/" mdlName <> ".hs"
    (pre, post) = setup $ moduleSetup mdl
    body =
      T.unlines
        $ [ T.unlines pre
          , "{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}"
          , "module DocTest." <> mdlName <> " where"
          , "import qualified DocTest"
          , "import Test.Tasty(TestTree,testGroup)"
          , "import " <> mdlName
          , T.unlines post
          , "tests :: IO TestTree"
          , "tests = testGroup "
          <> T.pack (show mdlNameS)
          <> " <$>"
          <> " sequence ["
          <> (T.intercalate "," . tabs . map content . concat $ moduleContent
               mdl
             )
          <> "]"
          ]
  writeModule path body
 where
  setup = maybe ([], []) (partitionEithers . map setupLine)
  setupLine (Located l (Example s [])) = asSetup s
  asSetup ('l' : 'e' : 't' : d) = Right $ T.strip (T.pack d)
  asSetup (parseOpts -> Just opts) =
    Left $ "{-# LANGUAGE " <> T.intercalate "," opts <> "#-}"
  asSetup s = Right $ T.pack s
  content (Located l (Example s exp)) = T.unwords
    [ "DocTest.test"
    , T.pack $ show $ show l
    , result exp
    , "(DocTest.asPrint("
    , T.pack s
    , "))"
    ]
  content (Located l (Property p)) = T.unwords
    ["DocTest.testProp", T.pack $ show $ show l, T.unwords ["(", T.pack p, ")"]]
  -- result = T.pack . show . unlines . map expect
  result = T.pack . show . map expect
  expect (ExpectedLine [LineChunk s]) = T.pack s
  expect (WildCardLine              ) = "WILD"

writeModule path body = do
  createDirectoryIfMissing True (takeDirectory path)
  T.writeFile path body


exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
  in  filePath `excludes` mdls
-- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)

tabs = map tab
tab = ("  " <>)

x = parseOpts ":set -XBinaryLiterals  -XOverloadedStrings"

parseOpts = parseMaybe languageOpts

languageOpts :: Parser [T.Text]
languageOpts =
  string ":set" *> many (space *> string "-X" *> (T.pack <$> many letterChar))

type Parser = Parsec Void String
