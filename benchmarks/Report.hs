{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}

module Report where

import Control.DeepSeq
import Control.Monad
import Criterion.IO
import Criterion.Types
import Data.List
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics
import Statistics.Types
import System.Directory
import System.FilePath
import Text.Printf
import Data.Bifunctor  
import Data.Char

-- | Map test names to test measures
type Measures = M.Map String Measure

data Measure = Measure
  { mTest :: String -- ^Test name, with the format <test-kind>/<test-obj>-<test-pkg>
  , mValue :: Double -- ^Execution time (in ms or bytes, depending on the test)
  } deriving (Show, Read, Eq, Generic, NFData, Ord)

{- | 
>>> import Data.List
>>> let m = Measure "deserialization (time)/BinTree Direction-binary" 1989
>>> let m2 = Measure "serialization (time)/BinTree Direction-binary" 1004
>>> let m3 = Measure "size (bytes)/BinTree Direction-binary" 6291455
>>> let m4 = Measure "size (bytes)/Cars-binary" 301455
>>> let m5 = Measure "size (bytes)/Cars-flat" 300000
>>> let ms = measures [m,m2,m3,m4,m5]

>>> mKindPkg m
("deserialization (time)/BinTree Direction","binary")

>>> mKindPkgVal m
("deserialization (time)/BinTree Direction",("binary",1989.0))

>>> mKind m
"deserialization (time)/BinTree Direction"

>>> mObj m
"BinTree Direction"

>>> mObjSub m
"BinTree Direction-binary"

>>> mSub m
"binary"

>>> allOf mObj ms
["BinTree Direction","Cars"]

>>> allOf mType ms
["deserialization (time)","serialization (time)","size (bytes)"]

>>> summaryTable ms
[("BinTree Direction",[("deserialization (time)",[(1989.0,[Measure {mTest = "deserialization (time)/BinTree Direction-binary", mValue = 1989.0}])]),("serialization (time)",[(1004.0,[Measure {mTest = "serialization (time)/BinTree Direction-binary", mValue = 1004.0}])]),("size (bytes)",[(6291455.0,[Measure {mTest = "size (bytes)/BinTree Direction-binary", mValue = 6291455.0}])])]),("Cars",[("size (bytes)",[(300000.0,[Measure {mTest = "size (bytes)/Cars-flat", mValue = 300000.0}]),(301455.0,[Measure {mTest = "size (bytes)/Cars-binary", mValue = 301455.0}])])])]

>>> renderTable ms 
"|Dataset\\Measure|deserialization|serialization|size|\n| ---| ---| ---| ---|\n|BinTree Direction|[binary](https://hackage.haskell.org/package/binary)|[binary](https://hackage.haskell.org/package/binary)|[binary](https://hackage.haskell.org/package/binary)|\n|Cars|||[flat](https://hackage.haskell.org/package/flat),[binary](https://hackage.haskell.org/package/binary)|\n"

>>> addTransfers_ ms
fromList [("deserialization (time)/BinTree Direction-binary",Measure {mTest = "deserialization (time)/BinTree Direction-binary", mValue = 1989.0}),("serialization (time)/BinTree Direction-binary",Measure {mTest = "serialization (time)/BinTree Direction-binary", mValue = 1004.0}),("size (bytes)/BinTree Direction-binary",Measure {mTest = "size (bytes)/BinTree Direction-binary", mValue = 6291455.0}),("size (bytes)/Cars-binary",Measure {mTest = "size (bytes)/Cars-binary", mValue = 301455.0}),("size (bytes)/Cars-flat",Measure {mTest = "size (bytes)/Cars-flat", mValue = 300000.0}),("transfer [10 MBits] (time)/BinTree Direction-binary",Measure {mTest = "transfer [10 MBits] (time)/BinTree Direction-binary", mValue = 8026.164}),("transfer [100 MBits] (time)/BinTree Direction-binary",Measure {mTest = "transfer [100 MBits] (time)/BinTree Direction-binary", mValue = 3496.3164}),("transfer [1000 MBits] (time)/BinTree Direction-binary",Measure {mTest = "transfer [1000 MBits] (time)/BinTree Direction-binary", mValue = 3043.33164})]
-}
renderTable :: Measures -> String
renderTable ms = 
  let tests = allOf mObj ms
      kinds = allOf mType ms
      vals = allOf mSub ms
      lines s = s : map (\t -> showPkgs $ map (\v -> tos t s v ms) vals) kinds
  in unlines . map mdRow $ ("Dataset\\Measure":map short kinds) : replicate (length kinds+1) " ---" : map lines tests
    where
      pkgVals = map snd . tops . sort . catMaybes . map ((\m -> (mValue m,pkgRef $ mSub m)) <$>)
      tops [] = []
      tops hs = let limit = fst (head hs) * 1.3 in takeWhile (\e -> fst e <= limit) hs
      showPkgs = intercalate "," . pkgVals 
      mdRow vs = concat["|",intercalate "|" vs,"|"]
      pkgRef name = concat ["[",name,"](https://hackage.haskell.org/package/",name,")"]
      short = unwords . init . words 

tos :: String -> String -> String -> Measures -> Maybe Measure
tos t o s ms = M.lookup (concat[t,"/",o,"-",s]) ms

summaryTable :: M.Map k Measure
                      -> [(String, [(String, [(Double, [Measure])])])]
summaryTable = map (second $ (map (second $ by mValue) . by mType)) . by mObj . M.elems

addTransfers :: FilePath -> IO ()
addTransfers workDir = addMeasures__ workDir addTransfers_ >> return ()

addTransfers_ :: Measures -> Measures
addTransfers_ ms =
  let tests = allOf mObjSub ms
      addT :: Measures -> String -> Measures
      addT ms o =
        fromMaybe ms $
        (\ser des siz ->
           let add1 :: Double -> Measures -> Measures
               add1 megaBits ms =
                 let trans =
                       concat
                         [ "transfer ["
                         , show (round megaBits :: Integer)
                         , " MBits] (time)/"
                         , o
                         ]
                     time =
                       mValue ser + mValue des +
                       mValue siz * 8 / (megaBits * 1000)
                  in add (Measure trans time) ms
            in add1 1000 (add1 100 (add1 10 ms))) <$>
        M.lookup ("serialization (time)/" ++ o) ms <*>
        M.lookup ("deserialization (time)/" ++ o) ms <*>
        M.lookup ("size (bytes)/" ++ o) ms
   in foldl addT ms tests

add :: Measure -> Measures -> Measures
add m = M.insert (mTest m) m

mKindPkg :: Measure -> (String, String)
mKindPkg m =
  let (k, _:p) = break (== '-') $ mTest m
   in (k, p)

mKind :: Measure -> String
mKind = fst . mKindPkg

mType :: Measure -> String
mType = brk1 '/' . mKind

mObj :: Measure -> String
mObj = brk2 '/' . mKind

mObjSub :: Measure -> String
mObjSub = brk2 '/' . mTest

mSub :: Measure -> String
mSub = brk2 '-' . mObjSub

brk1 :: Eq a => a -> [a] -> [a]
brk1 sep = fst . break (== sep)

brk2 :: Eq a => a -> [a] -> [a]
brk2 sep = tail . snd . break (== sep)

mKindPkgVal :: Measure -> (String, (String, Double))
mKindPkgVal m =
  let (k, p) = mKindPkg m
   in (k, (p, mValue m))

byTestKind :: Measures -> [(String, (String, Double))]
byTestKind = map mKindPkgVal . M.elems

allKinds :: Measures -> [String]
allKinds = allOf mKind

allOf :: Eq a1 => (a2 -> a1) -> M.Map k a2 -> [a1]
allOf f = nub . map f . M.elems

allTests :: [(String, (String, Double))] -> [(String, [(String, Double)])]
allTests = -- by fst
  sort .
   map (\g -> (fst . head $ g, map snd g)) . groupBy (\a b -> fst a == fst b)

-- order by dimension
by :: (Ord a, Ord t) => (t -> a) -> [t] -> [(a, [t])]
by f = sort . map (\g -> (f $ head g, g)) . groupBy (\a b -> f a == f b)

toMeasures :: [Report] -> Measures
toMeasures = measures . map toMeasure

measures :: [Measure] -> Measures
measures = M.fromList . map (\m -> (mTest m, m))

toMeasure :: Report -> Measure
toMeasure r =
  Measure (reportName r) ((1000 *) . estPoint . anMean . reportAnalysis $ r)

deleteMeasures :: FilePath -> IO ()
deleteMeasures = removeFile . measuresFile

updateMeasures :: FilePath -> IO ()
updateMeasures = void . updateMeasures_

updateMeasures_ :: FilePath -> IO (Measures, Measures, Measures)
updateMeasures_ dir = do
  m' <- readCriterionMeasures dir
  addMeasures_ dir m'

addMeasures_ :: FilePath -> Measures -> IO (Measures, Measures, Measures)
addMeasures_ dir m' = addMeasures__ dir (const m')

addMeasures__ ::
     FilePath -> (Measures -> Measures) -> IO (Measures, Measures, Measures)
addMeasures__ dir f = do
  !m <- readMeasures dir
  let m' = f m
  let m'' = M.union m' m
  writeMeasures dir m''
  return (m, m', m'')

addMeasures :: FilePath -> String -> [(String, Double)] -> IO ()
addMeasures dir name ms =
  void $
  addMeasures_
    dir
    (M.fromList $
     map
       (\(pkg, val) ->
          let n = concat [name, "-", pkg]
           in (n, Measure n val))
       ms)

readMeasures :: FilePath -> IO Measures
readMeasures dir = do
  let f = measuresFile dir
  fe <- doesFileExist f
  if not fe
    then return M.empty
    else (force . read) <$> readFile f

writeMeasures :: FilePath -> Measures -> IO ()
writeMeasures dir = writeFile (measuresFile dir) . show

readCriterionMeasures :: FilePath -> IO Measures
readCriterionMeasures dir = toMeasures <$> readReports (reportsFile dir)

measuresFile :: FilePath -> FilePath
measuresFile dir = dir </> "measures"

reportsFile :: FilePath -> FilePath
reportsFile dir = dir </> "report.json"

reportsMDFile :: FilePath -> FilePath
reportsMDFile dir = dir </> "report.md"

printMeasures :: FilePath -> IO ()
printMeasures dir = reportMeasures_ dir >>= putStrLn

printSummary :: FilePath -> (String -> Bool) -> IO ()
printSummary dir f = readMeasures dir >>= putStrLn . renderTable . M.filter (f . mTest)

reportMeasures :: FilePath -> IO ()
reportMeasures dir =
  reportMeasures_ dir >>=
  writeFile (reportsMDFile dir) .
  ("Results that are within 30% of the best result are displayed in **bold**.\n\n" ++)

reportMeasures_ :: FilePath -> IO String
reportMeasures_ dir = reportMeasures__ (const True) <$> readMeasures dir

printMeasuresCurrent :: (Measures, Measures, Measures) -> IO ()
printMeasuresCurrent (_, m', m'') =
  let currentKinds = allKinds m'
   in printMeasuresAll_ (\ts -> fst ts `elem` currentKinds) m''

printMeasuresAll_ :: ((String, [(String, Double)]) -> Bool) -> Measures -> IO ()
printMeasuresAll_ f
 = putStrLn . reportMeasures__ f

reportMeasures__ :: ((String, [(String, Double)]) -> Bool) -> Measures -> String
reportMeasures__ f =
  concatMap (uncurry report) . filter f . allTests . byTestKind

printMeasuresDiff :: (Measures, Measures, Measures) -> IO ()
printMeasuresDiff (m, m', _) =
  mapM_ (\(n, d) -> putStrLn (unwords [n, show d, "%"])) . M.toList $
  M.intersectionWith
    (\a b -> round ((mValue b / mValue a - 1) * 100) :: Int)
    m
    m'

readReports :: FilePath -> IO [Report]
readReports jsonReportFile = do
  fe <- doesFileExist jsonReportFile
  if not fe
    then return [] -- M.empty
    else do
      er <- readJSONReports jsonReportFile
      return $
        case er of
          Right (_, _, reports) -> reports
          _ -> [] -- M.empty

report :: String -> [(String, Double)] -> String
report _ [] = []
report name rs
 =
  let (_, rss) = report_ rs
      width = maximum . map (length . fst) $ rs
      out = ["| ---| ---|","| package | performance |","",unwords ["####",name, "(best first)"]] 
      -- out = ["| ---| ---| ---|","| package | measure | relative measure |","",unwords ["####",name, "(best first)"]] 
   in unlines . reverse $
      "" :
      foldl'
        (\out (n, r, a) ->
           let bold n = concat ["**",n,"**"] 
               marked n = -- fix prob with github bold display
                 if r <= 1.3 
                   then if isSpace (head n) 
                        then let (n1,n2) = span isSpace n in n1 ++ bold n2 
                        else let (n1,n2) = span (not . isSpace) n in bold n1 ++ n2 
                   else n
            in unwords
                 [ "|"
                 ,marked (printString width n) -- pkg name
                 ,"|"
                 -- , printf "%11.1f" a -- absolute measure
                 -- ,"|"
                 , marked (printDouble r) -- relative measure
                 , "|"] :
               out)
        out
        rss

report_ :: (Fractional c, Ord c) => [(a, c)] -> ((a, c), [(a, c, c)])
report_ rs =
  let rss = sortOn snd rs
      best = snd . head $ rss
   in (head rss, map (\(n, v) -> (n, v / best, v)) rss)

printDouble :: Double -> String
printDouble = printf "%7.1f"

printInt :: Double -> String
printInt = printf "%.0f"

printString :: Int -> String -> String
printString width = printf ("%-" ++ show width ++ "s")
