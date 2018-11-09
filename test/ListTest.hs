{-# LANGUAGE CPP #-}
module Main where

import Data.Flat

longBools = replicate 1000000 True

main = print $ flat longBools