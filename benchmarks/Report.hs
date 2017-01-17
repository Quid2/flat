{-# LANGUAGE ViewPatterns #-}
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

printReports :: [Report] -> IO ()
printReports = mapM_ (\(tst,ms) -> report tst "Time" "mSecs" ms) . allTests . summaryReports

allTests = sort . map (\g -> (fst . head $ g,map snd g)) . groupBy (\a b -> fst a == fst b)

printDiffReports :: [Report] -> [Report] -> IO ()
printDiffReports reports reports' =
  mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) $ M.intersectionWithKey (\k a b ->(k,round ((b/a-1)*100))) (diffReports reports) (diffReports reports')

diffReports :: [Report] -> M.Map String Double
diffReports = (M.fromList . map reportSummary0)

summaryReports :: [Report] -> [([Char], ([Char], Double))]
summaryReports = (map reportSummary)

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

reportSummary r =
  let (k,_:p) = break (== '-') $ reportName r
  in  (k,(p,(1000 *) . estPoint . anMean . reportAnalysis $ r))

reportSummary0 r =
  let n = reportName r
  in (n,(1000 *) . estPoint . anMean . reportAnalysis $ r)

-- tstEnc/floats-store -> -- tstEnc/floats store
reportSummary2 r =
  let (k,_:p) = break (== '-') $ reportName r
  in  (k,p,(1000 *) . estPoint . anMean . reportAnalysis $ r)

--data Measure = Measure {mSource::String,mValue::Double} deriving (Show,Eq)

report :: String -> String -> String -> [(String,Double)] -> IO ()
report _ _ _ [] = return ()
report name prop unit rs = do
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
