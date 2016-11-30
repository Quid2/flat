{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-
 Requirements: compare different packages and version of different packages with different options on multiple tests, capturing speed/size/memory or any other parameter.

 What it does:
 Uses a specification of all the flags combination we are interested in (this also allows to test different packages)
 If the result of the combination are not there yet
  it runs benchsmall with it and add the results to the DB
  otherwise it just retrieve the results from the db
 Then it shows the lot.

How to Use:
Set benchsmall/Main(main) to the test you want to use:
main = mainProfileDecode

quid2.cabal must have:
   cpp-options:
in both Library and test-suite entries
-}
module Main where
-- | Run tests with different cabal options
import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Text           as T
import           System.Directory
import           System.Process
-- import           Data.Text.IO as T
import           Control.Applicative
import qualified Data.Map            as M
import           Data.String
import           System.FilePath
import           Text.Regex
-- import Data.ByteString
import           Text.Printf
-- import Data.List.Util

t = main

type DB = [(String    -- context (flags)
           ,Results
           )]

type Results = [(String  -- name of test
                ,Double  -- time
                )]

type DBByTest = [((String -- test
                  ,String -- pars
                  ),Double)]

emptyDB :: DB
emptyDB = [] -- M.empty

data Tree a = Add [Tree a] | Mul [Tree a] | Val a deriving Show

instance IsString (Tree String) where fromString = Val

-- Alternatives
os = map removeOverloaded . asList $ alts

alts :: Tree String
alts =
       m ["ENC_8","LIST_BIT" -- defaults
          ,a ["OP_ENC","OP_ENCDEC"] -- PROBIn: ,"OP_DEC"]--,"ARRDEC_DIRECT"]] -- ,"OP_ENCDEC"]
          ,a [m ["PKG_QUID2"
                --,a ["ENC_8","ENC_64"]
                ,a ["LIST_BIT",m ["LIST_BYTE",a ["ENCLIST_GO","ENCLIST_FOLDL2","ENCLIST_DIV"]]]]
             ,"PKG_CBOR"
             ,"PKG_BINARY"]
         ]
{-
Pkg_quis2 * (op_dec + (op_enc * (enc_8+enc_64) * (list_bit+list_byte)) + pkgCBOR
-}

m = Mul
a = Add
v :: String -> Tree String
v = Val


removeOverloaded :: [String] -> [String]
removeOverloaded = reverse . nubBy (\a b -> hdr a == hdr b) . reverse

h :: Bool
h = hdr "PKG_QUID2" == "PKG"
hdr l = fst . splitFor '_' $ l

splitFor s l = flip splitAt l . fromJust . findIndex (== s) $ l

asList :: Tree a -> [[a]]
asList  (Add ts) = concat . map asList $ ts
asList  (Mul ts) = map concat . sequence . map asList $ ts
asList  (Val a) = [[a]]

{-
os :: [[String]]
os = sequence alts

alts = [opts $ StrOption "PKG" ["BINARY","CBOR","QUID2"]
       ,opts $ StrOption "OP" ["ENCDEC"]
       --,opts $ StrOption "ARRDEC" ["DIRECT","REVERSE"]
-}

dir = "/Users/titto/workspace/flat" --
dataDir = dir </> "benchsmall/data"
tmpDir = "/tmp"
dbFile = dataDir </> "DB"
cabalFile = dir </> "quid2.cabal"
cabalPrevFile = cabalFile ++ ".prev"
testResults = dir </> "benchsmall.totals.txt"

-- Execute tests with different options, if not executed previously.
main = do
  y <- doesFileExist dbFile
  db <- drop 1 <$> if y
                   then read <$> readFile dbFile
                   else return emptyDB

  callCommand $ unwords ["cp",cabalFile,cabalPrevFile]
  cabalTxt <- readFile cabalFile
  print $ length cabalTxt -- to force close of file
  print "Executing all" >> print os
  db' <- forM (take 400 os) $ \ o -> let k = key o in (k,) <$> do
         case lookup k db of
           Just r -> return r
           Nothing -> doTest cabalTxt o
  writeFile dbFile (show db')
  let db'' = dbByTest db'
  -- print by test
  mapM_ print . intercalate [""] . map frm . map (sortBy byTime) . groupBy eqTest . sortBy byTest $ db''
  -- print all together
  -- mapM_ print . frm . concat . map (sortBy byTime) $ db''


byTime a b = compare (snd a) (snd b)

byTest a b = compare (ff a) (ff b)

eqTest a b = ff a == ff b

ff = fst . fst

frm l@((_,t0):xs) = map (\(k,t) -> unwords [concat["",printf "%.1f" $ (t/t0),""],show k]) l

dbByTest :: DB -> DBByTest
dbByTest = concatMap (\(pars,rs) -> map (\(tstName,tstTime) -> ((tstName,pars),tstTime)) rs)


doTest cabalTxt o = do
  -- let prof = dataDir </> concat ["benchsmall",plain o,".prof"]
  print $ unwords ["Executing",show o]
  writeFile cabalFile $ replaceCPP o cabalTxt
  {-  PROB: total time measured by profiler depends on how many cost centres are used!
          callCommand $ concat [unwords["cd",dir,";"]
                              ,"cabal install --enable-executable-profiling -p;"
                              ,unwords [dir </> ".cabal-sandbox/bin/benchsmall","+RTS -p;"]
                              ,unwords ["mv benchsmall.prof",prof,";"]
                              ]
         profTxt <- readFile prof
-}
  let timef = tmpDir </> concat ["testTime",plain o]
  mapM callCommand $ [concat [unwords["cd",dir,";"],"cabal install;"]
                     ,(dir </> ".cabal-sandbox/bin/benchsmall") ++ " +RTS -l"
                     ,unwords["cd",dir,";ghc-events-analyze benchsmall.eventlog;"]
                     ]
  --totalTime2 <$> readFile timef
  r <- getTestResults <$> readFile testResults
  print r -- to force read file
  return r


key = unwords -- . sort
plain = replace "=" "_" . options_ "_"
cpp = options_ " -D"
options_ p = concat . map (\o-> if length o > 0 then concat [p,o] else "")

replace a b s = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack s)

-- data Options = forall a. Opts a => Options [a]

class Opts v where opts :: v -> [String]

-- instance Opts [String] where opts s = s

data Optional = Optional String
instance Opts Optional where opts (Optional v) = ["",v]

data NumOption = NumOption String Int Int
instance Opts NumOption where opts (NumOption s l h) = map ((s++) . show) [l..h]

data ValOption = ValOption String Int Int
instance Opts ValOption where opts (ValOption s l h) = map (\v -> concat [s,"=",show v])  [l..h]

data StrOption = StrOption String [String]
instance Opts StrOption where opts (StrOption s ss) = map (\v -> concat [s,"_",v]) ss

replaceCPP o s = subRegex (mkRegex "cpp-options:.*\n") s (concat["cpp-options:",cpp o,"\n"])

totalTime :: String -> Double
totalTime s = let Just [n] = matchRegex (mkRegex "total time  =(.*)secs") s in read n

totalTime2 :: String -> Double
totalTime2 s = let Just ([ms,ss]::[String]) = matchRegex (mkRegex "\nreal\t([0-9]+)m(.*)s.*") s
               in (fromIntegral (read ms::Int)) * 60 + (read ss::Double)

-- j = totalTime2 "\nreal\t12m3.059s\nuser\t0m2.908s\nsys\t0m0.093s\n"

-- x = matchRegex (mkRegex "\nreal\t0m(.*)s.*") "\nreal\t0m3.059s\nuser\t0m2.908s\nsys\t0m0.093s\n"

x = do
  f <- readFile testResults
  -- print t
  return . getTestResults $ f

getTestResults :: String -> Results
getTestResults = map (\r -> let [n,_,t] = words r in (n,read $ init t)) . lines . fromJust . between "USER EVENTS (user events are corrected for GC)\n" "TOTAL"



