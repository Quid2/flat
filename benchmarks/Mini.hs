-- Mini Benchmark
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables ,CPP #-}

module Main where
import           Control.Concurrent
import           Control.DeepSeq
import           Criterion.IO
import           Criterion.Main
import           Criterion.Types
import qualified Data.ByteString    as B
import           Data.Flat
import qualified Data.Map           as M
import           Report
import           System.Directory
import           System.FilePath
import           System.Process     (callCommand)
import           Test.Data.Flat
import           Test.Data.Values
import           Test.E
import           Test.E.Flat
import Data.List
import Common

#ifdef ETA_VERSION    
import Data.Function(trampoline)
import GHC.IO(trampolineIO)
#else
trampoline = id
trampolineIO = id
#endif

bname = bench . benchName

setupEnv = do
  -- print treeLarge
  let small = replicate 1000 (1 :: Int)
  let v1 = (E3_3, B.pack [193])
  let v2 = (E16_16, B.pack [241])
  -- let v3 = valTest longBoolListT
  let v3 = "" -- (E256_256, B.pack [255, 1])
  let v4 = valTest treeLargeT
  -- print $ "Size Tree is " ++ show (getSize treeLarge)
  -- print $ "Size Tree " ++ show (5999999 == getSize treeLarge)
  -- print $ "Size Tree " ++ show (6999999 == getSize treeLarge)
  -- print $ unwords ["Size S3 ",show ( == getSize S1treeLarge)
  -- print v4
  -- putStrLn $ take 1000 . show $ (unflat $ enc v4 :: Either DecodeException ((Tree N)))
  -- big <-
  --   map length . words <$>
  --   readFile "/Users/titto/workspace/flat/benchmarks/Simple.hs"
  return (v1, v2, v3, v4)

projDir = "."

workDir = projDir </> "benchmarks/data"

tmpDir = "/tmp"

main
 = trampolineIO $ do
  createDirectoryIfMissing True workDir
  mainBench_ (reportsFile workDir)
  prtMeasures

prtMeasures = do  
    -- delete measures to avoid eta read bug
-- #ifdef ETA_VERSION    
--     deleteMeasures workDir
-- #endif
    ms <- updateMeasures_ workDir
    printMeasuresDiff ms
    -- printMeasuresAll ms
    printMeasuresCurrent ms

mainBench_ jsonReportFile =
  defaultMainWith (defaultConfig {
   jsonFile = Just jsonReportFile
   -- ,verbosity=Quiet -- avoid trouble with unit of measure character
    }) [runtime]
  where
    runtime =
      env setupEnv $ \ ~(v1, v2, v3, v4 ) ->
        bgroup "basic" [
          basicTest "large Tree" v4
          -- ,basicTest "large list" v3
        ]

basicTest name v =         
        bgroup
        name
        [ bname "size" $whnf (getSize . val) v
         ,bname "enc" $whnf (\tv -> B.length (bs (encod (val tv))) == encLen tv) v
         ,bname "dec eq" $ whnf (\tv -> val tv == val tv) v
         ,bname "dec" $ whnf (\tv -> decod (enc tv) == Right (val tv)) v
        ]

    --compilation = bgroup "compilation-basic" [bench "compile" $ nfIO comp]

-- comp = do
--   callCommand $ concat ["touch ", projDir </> "test/Test/E/Flat.hs"]
--   callCommand $
--     concat
--       ["cd ", projDir, ";stack ghc -- -isrc  -itest -O test/Test/E/Flat.hs"]

fromRight (Right a) = a
-- fromRight (Left e) = error $ show e
