module Main where

import Criterion
import Criterion.Main
import Data.Functor

import All (solutions)

name :: (Int, Int) -> String
name (d, p) = "Day " <> show (d + 1) <> ", Part " <> show (p + 1)

benches :: [Benchmark]
benches = zip [0 :: Int ..] solutions
  <&> (\(n, io) -> bench (name $ n `divMod` 2) (nfIO io))

main :: IO ()
main = defaultMain benches
