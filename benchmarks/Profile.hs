{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Flat
import Debug.Trace (traceEventIO)
import Test.Data(Direction(..),Tree(..))
import Data
import Test.Data.Values
import Test.Data.Flat
import Data.Word
import Weigh
{-
Profiling:
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -caf-all" --exec "flat-profile +RTS -p" --file-watch --exec "profiteur flat-profile.prof"

To get a stack error (to detect space leaks)
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -caf-all -rtsopts" --exec "flat-profile +RTS -xc -K1k" --file-watch


Profiling using eventlog (not useful unless we mark with 'event'):
cd /Users/titto/workspace/flat;stack install --file-watch --exec "flat-profile +RTS -l" --exec "ghc-events-analyze --totals flat-profile.eventlog" --exec "cat flat-profile.totals.txt"

-}

--main = print $ encode Four -- (Node (Leaf Four) (Leaf Three))


main = do
  -- forceCafs
  --print $ L.length (flat treeN33Large)
  --print $ L.length (flat treeNNNLarge)
  -- p 500000 treeVarious
  -- p 5 treeN33Large
  -- p 5000000 vw
  -- p 10000 unicodeStr

  -- let v = nativeList
  --let v = treeNLarge
  let v = treeNNNLarge
  -- let v = [1::Word64 .. 4000000]
  -- evaluate $ force' (NF v)
  -- putStrLn . commas . length $ v

  putStrLn . commas . L.length . flat $ v

  where p n = print . sum . map (L.length . flat) . take n . repeat


--main = testDirectionList

-- testDirectionList = do
--   let es = take 10000 . concat . repeat $ [North,South,East,West,Center]
--   print $ unflat (flat es) == Right es

-- testIntList = do
--   let es = map flat [-100000..100001::Int]
--   -- mapM_ (\_ -> dec e :: IO (Decoded Int)) [1..1000000]
--   let r = sum . map (\e -> let Right v = unflat e in (v::Int)) $ es
--   print $ r == 100001


--enc = evaluate . L.length . flat
--dec = evaluate . unflat

event :: String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)
