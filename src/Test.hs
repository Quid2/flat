{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import Flat
import Flat.Instances.Vector
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

data Foo =
    A
  | B (Vector Double)
  | C (Vector Double) (Vector Double)
  deriving (Generic, Flat, Show)

test :: IO ()
test = do
  print $ unflat @Foo . flat $ A
  print $ unflat @Foo . flat $ B (V.fromList [7,8])
  print $ unflat @Foo . flat $ C (V.fromList [7,8]) (V.fromList [9, 10])