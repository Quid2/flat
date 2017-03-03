{-# LANGUAGE ViewPatterns ,DeriveGeneric,DeriveAnyClass ,BangPatterns #-}
module Report where
import           Criterion.IO
import           Criterion.Types
import           Data.Bifunctor
import           Data.List
import qualified Data.Map                        as M
import           Data.Ord
import           Statistics.Resampling.Bootstrap
import           System.Directory
import           Text.Printf
import System.FilePath
import Control.DeepSeq
import GHC.Generics

type Measures = M.Map String Measure

data Measure = Measure {mTest::String,mValue::Double} deriving (Show,Read,Eq,Generic,NFData)

mKindPkg :: Measure -> (String, String)
mKindPkg m = let (k,_:p) = break (== '-') $ mTest m in (k,p)

mKind :: Measure -> String
mKind = fst . mKindPkg

mKindPkgVal :: Measure -> (String, (String, Double))
mKindPkgVal m = let (k,p) = mKindPkg m in (k,(p,mValue m))

byTestKind = map mKindPkgVal . M.elems

toMeasures :: [Report] -> Measures
toMeasures = M.fromList . map (\r-> let m = toMeasure r in (mTest m,m))

toMeasure :: Report -> Measure
toMeasure r = Measure (reportName r) ((1000 *) . estPoint . anMean . reportAnalysis $ r)

updateMeasures :: FilePath -> IO (Measures,Measures,Measures)
updateMeasures dir = do
  !m <- readMeasures dir
  m' <- readLastMeasures dir
  let m'' = M.union m' m
  writeMeasures dir m''
  return (m,m',m'')

readMeasures :: FilePath -> IO Measures
readMeasures dir =  do
  let f = measuresFile dir
  fe <- doesFileExist f
  if not fe
    then return M.empty
    else (force . read) <$> readFile f

writeMeasures :: FilePath -> Measures -> IO ()
writeMeasures dir = writeFile (measuresFile dir) . show

readLastMeasures :: FilePath -> IO Measures
readLastMeasures dir = toMeasures <$> readReports (reportsFile dir)

measuresFile dir = dir </> "measures"
reportsFile dir = dir </> "report.json"

printMeasuresAll :: (Measures,Measures,Measures) -> IO ()
printMeasuresAll = printMeasuresAll_ (const True)

printMeasuresCurrent :: (Measures,Measures,Measures) -> IO ()
printMeasuresCurrent ms@(_,m',_) =
  let currentKinds = allKinds m'
  in printMeasuresAll_ (\ts -> fst ts `elem` currentKinds) ms

printMeasuresAll_ f (_,m',m'') =
  let currentKinds = allKinds m'
  in mapM_ (\(tst,ms) -> report tst "Time" "mSecs" ms) . filter f . allTests . byTestKind $ m''

allKinds = nub . map mKind . M.elems

-- printReports :: [Report] -> IO ()
-- printReports = mapM_ (\(tst,ms) -> report tst "Time" "mSecs" ms) . allTests . summaryReports

allTests
  :: [(String, (String, Double))] -> [(String, [(String, Double)])]
allTests = sort . map (\g -> (fst . head $ g,map snd g)) . groupBy (\a b -> fst a == fst b)

printMeasuresDiff :: (Measures,Measures,Measures) -> IO ()
printMeasuresDiff (m,m',_) =
--  mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) . M.toList $ M.intersectionWithKey (\k a b ->(k,round ((mValue b/mValue a-1)*100))) m m'
  mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) . M.toList $ M.intersectionWith (\a b -> round ((mValue b/mValue a-1)*100)) m m'

-- printDiffReports :: [Report] -> [Report] -> IO ()
-- printDiffReports reports reports' =
--   mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) $ M.intersectionWithKey (\k a b ->(k,round ((b/a-1)*100))) (diffReports reports) (diffReports reports')

-- diffReports :: [Report] -> M.Map String Double
-- diffReports = (M.fromList . map reportSummary0)

-- summaryReports :: [Report] -> [(String, (String, Double))]
-- summaryReports = (map reportSummary)

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

-- reportSummary r =
--   let (k,_:p) = break (== '-') $ reportName r
--   in  (k,(p,(1000 *) . estPoint . anMean . reportAnalysis $ r))

-- reportSummary0 r =
--   let n = reportName r
--   in (n,(1000 *) . estPoint . anMean . reportAnalysis $ r)

-- tstEnc/floats-store -> -- tstEnc/floats store
-- reportSummary2 r =
--   let (k,_:p) = break (== '-') $ reportName r
--   in  (k,p,(1000 *) . estPoint . anMean . reportAnalysis $ r)

report :: String -> String -> String -> [(String,Double)] -> IO ()
report _ _ _ [] = return ()
report name prop unit rs = do
  print rs
  let (best,rss) = report_ rs
  let width = maximum . map (length . fst) $ rs
  putStrLn $ unwords [name,"ordered by",prop,"("++fst best++":",printInt (snd best),unit++")"]
  mapM_ (\(n,v) -> putStrLn $ unwords [printString width n,printDouble v]) rss
  putStrLn ""

report_ rs =
  let
    rss = sortBy (comparing snd) rs
    best = snd . head $ rss
  in (head rss, map (second (\v -> v / best)) rss)

printDouble :: Double -> String
printDouble = printf "%5.1f"

printInt :: Double -> String
printInt = printf "%.0f"

printString :: Int -> String -> String
printString width = printf ("%-"++show width++"s")
