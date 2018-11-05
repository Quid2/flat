-- Mini Benchmark
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.DeepSeq
import           Criterion.IO
import           Criterion.Main
import           Criterion.Types
import qualified Data.ByteString    as B
import           Data.Flat
import           Report
import           System.FilePath
import           System.Process     (callCommand)
import           Test.Data.Flat
import           Test.Data.Values
import           Test.E
import           Test.E.Flat

data ValTest a = ValTest
  { name   :: String
  , val    :: a
  , enc    :: Encoded a
  , encLen :: !Int
  } deriving (Generic, NFData, Show)

data Encoded a = Encoded
  { bs :: B.ByteString
  } deriving (Generic, NFData, Show)

encod :: Flat a => a -> Encoded a
encod = Encoded . flat

decod :: Flat a => Encoded a -> Decoded a
decod (Encoded bs) = unflat bs

valTest (n, v) =
  let e = encod v
  in ValTest n v e (B.length $ bs e)

treeLarge :: Tree E16
treeLarge = mkTreeOf largeSize
treeLargeT = ("LargeTree",treeLarge)

setupEnv = do
  -- print treeLarge
  let small = replicate 1000 (1 :: Int)
  let v1 = (E3_3, B.pack [193])
  let v2 = (E16_16, B.pack [241])
  let v3 = v2
  -- let v3 = (E256_256, B.pack [255, 1])
  let v4 = valTest treeLargeT
  print $ "Size Tree is " ++ show (getSize treeLarge)
  print $ "Size Tree " ++ show (5999999 == getSize treeLarge)
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
 = do
  mainBench_ (reportsFile workDir)
  -- deleteMeasures workDir
  ms <- updateMeasures_ workDir
  printMeasuresDiff ms
    -- printMeasuresAll ms
  printMeasuresCurrent ms

mainBench_ jsonReportFile =
  defaultMainWith (defaultConfig {jsonFile = Just jsonReportFile}) [runtime]
  where
    runtime =
      env setupEnv $ \ ~(v1, v2, v3, v4 ) ->
        bgroup
          "runtime-basic"
          -- bench "dec" $ whnf (\(v, b) -> unflat b == Right v) v1
          --, bench "dec" $ whnf (\(v, b) -> unflat b == Right v) v2
          --, bench "dec" $ whnf (\(v, b) -> unflat b == Right v) v3
          -- bench "enc eq" $ whnf (\tv -> enc tv == encVal tv) v4
          [ bench "enc" $whnf (\tv -> B.length (bs (encod (val tv))) == encLen tv) v4
           ,bench "size" $whnf (getSize . val) v4
           ,bench "dec eq" $ whnf (\tv -> val tv == val tv) v4
           ,bench "dec" $ whnf (\tv -> decod (enc tv) == Right (val tv)) v4
          ]
    compilation = bgroup "compilation-basic" [bench "compile" $ nfIO comp]

comp = do
  callCommand $ concat ["touch ", projDir </> "test/Test/E/Flat.hs"]
  callCommand $
    concat
      ["cd ", projDir, ";stack ghc -- -isrc  -itest -O test/Test/E/Flat.hs"]

fromRight (Right a) = a
-- fromRight (Left e) = error $ show e
