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
import Control.Monad

type Measures = M.Map String Measure

data Measure = Measure {mTest::String,mValue::Double} deriving (Show,Read,Eq,Generic,NFData)

mKindPkg :: Measure -> (String, String)
mKindPkg m = let (k,_:p) = break (== '-') $ mTest m in (k,p)

mKind :: Measure -> String
mKind = fst . mKindPkg

mKindPkgVal :: Measure -> (String, (String, Double))
mKindPkgVal m = let (k,p) = mKindPkg m in (k,(p,mValue m))

byTestKind :: M.Map k Measure -> [(String, (String, Double))]
byTestKind = map mKindPkgVal . M.elems

toMeasures :: [Report] -> Measures
toMeasures = M.fromList . map (\r-> let m = toMeasure r in (mTest m,m))

toMeasure :: Report -> Measure
toMeasure r = Measure (reportName r) ((1000 *) . estPoint . anMean . reportAnalysis $ r)

updateMeasures :: FilePath -> IO ()
updateMeasures = void . updateMeasures_

updateMeasures_ :: FilePath -> IO (Measures, Measures, Measures)
updateMeasures_ dir = do
  m' <- readCriterionMeasures dir
  addMeasures_ dir m'

addMeasures_
  :: FilePath
     -> Measures
     -> IO (Measures, Measures, Measures)
addMeasures_ dir m' = do
  !m <- readMeasures dir
  let m'' = M.union m' m
  writeMeasures dir m''
  return (m,m',m'')

addMeasures
  :: FilePath
     -> String
     -> [(String, Double)]
     -> IO () -- Measures, Measures, Measures)
addMeasures dir name ms = void $ addMeasures_ dir (M.fromList $ map (\(pkg,val) -> let n = concat [name,"-",pkg] in (n,Measure n val)) ms)

readMeasures :: FilePath -> IO Measures
readMeasures dir =  do
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

printMeasures :: FilePath -> IO ()
printMeasures dir = readMeasures dir >>= printMeasuresAll_ (const True)

printMeasuresCurrent :: (Measures,Measures,Measures) -> IO ()
printMeasuresCurrent (_,m',m'') =
  let currentKinds = allKinds m'
  in printMeasuresAll_ (\ts -> fst ts `elem` currentKinds) m''

printMeasuresAll_
  :: ((String, [(String, Double)]) -> Bool)
     -> Measures -> IO ()
printMeasuresAll_ f m'' = mapM_ (uncurry report) . filter f . allTests . byTestKind $ m''

allKinds :: Measures -> [String]
allKinds = nub . map mKind . M.elems

allTests
  :: [(String, (String, Double))] -> [(String, [(String, Double)])]
allTests = sort . map (\g -> (fst . head $ g,map snd g)) . groupBy (\a b -> fst a == fst b)

printMeasuresDiff :: (Measures,Measures,Measures) -> IO ()
printMeasuresDiff (m,m',_) =
--  mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) . M.toList $ M.intersectionWithKey (\k a b ->(k,round ((mValue b/mValue a-1)*100))) m m'
  mapM_ (\(n,d) -> putStrLn (unwords [n,show d,"%"])) . M.toList $ M.intersectionWith (\a b -> round ((mValue b/mValue a-1)*100)::Int) m m'

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

report :: String -> [(String,Double)] -> IO ()
report _ [] = return ()
report name rs = do
  -- print rs
  let (_,rss) = report_ rs
  let width = maximum . map (length . fst) $ rs
  -- putStrLn $ unwords [name,"ordered by",prop,"("++fst best++":",printInt (snd best),unit++")"]
  putStrLn $ unwords [name,"(best first)"] -- package: "++fst best++" with ",printInt (snd best)++")"]
  -- putStrLn name -- ,"(Best package: "++fst best++" with ",printInt (snd best)++")"]
  mapM_ (\(n,v) -> putStrLn $ unwords [printString width n,printDouble v]) rss
  putStrLn ""

report_ :: (Fractional c, Ord c) => [(a, c)] -> ((a, c), [(a, c)])
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
