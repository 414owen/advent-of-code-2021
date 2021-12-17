{-# LANGUAGE ViewPatterns #-}

module Main where

import Criterion
import Criterion.Main
import System.Environment
import Text.Read
import Data.Functor

import All (solutions)
benches :: [Benchmark]
benches = zip [(0 :: Int)..] solutions
  <&> (\(n, io) -> bench (show $ n `divMod` 2) (nfIO io))

isBenchLine :: String -> Bool
isBenchLine _ = True

latest :: IO Int
latest = readFile "README.md"
  <&> lines
  <&> filter (/= "")
  <&> filter isBenchLine
  <&> length

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bench"] -> defaultMain benches
    [readMaybe -> Just n, readMaybe -> Just m] -> solutions !! ((n - 1) * 2 + m - 1)
    _          -> putStrLn "Invalid problem number!"
