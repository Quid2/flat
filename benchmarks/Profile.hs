import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Flat
import Debug.Trace (traceEventIO)

{-
Profiling:
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -caf-all" --exec "flat-profile +RTS -p" --file-watch


Profiling using eventlog (need to mark with event):
cd /Users/titto/workspace/flat;stack install --file-watch --exec "flat-profile +RTS -l" --exec "ghc-events-analyze --totals flat-profile.eventlog" --exec "cat flat-profile.totals.txt"

-}

main = do
  let es = map flat [-100000..100001::Int]
  -- mapM_ (\_ -> dec e :: IO (Decoded Int)) [1..1000000]
  let r = sum . map (\e -> let Right v = unflat e in (v::Int)) $ es
  print $ r == 100001


--enc = evaluate . L.length . flat
--dec = evaluate . unflat

event :: String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)
