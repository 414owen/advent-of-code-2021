{-# LANGUAGE ViewPatterns #-}

module Main where

import Criterion
import Criterion.Main
import System.Environment
import Text.Read
import Data.Functor

import qualified Ad01
import qualified Ad02
import qualified Ad03
import qualified Ad04
import qualified Ad05
import qualified Ad06
import qualified Ad07
import qualified Ad08
import qualified Ad09
import qualified Ad10
import qualified Ad11
import qualified Ad12
import qualified Ad13
import qualified Ad14
import qualified Ad15
import qualified Ad16
import qualified Ad17

solutions :: [IO ()]
solutions =
  [ Ad01.main1
  , Ad01.main2
  , Ad02.main1
  , Ad02.main2
  , Ad03.main1
  , Ad03.main2
  , Ad04.main1
  , Ad04.main2
  , Ad05.main1
  , Ad05.main2
  , Ad06.main1
  , Ad06.main2
  , Ad07.main1
  , Ad07.main2
  , Ad08.main1
  , Ad08.main2
  , Ad09.main1
  , Ad09.main2
  , Ad10.main1
  , Ad10.main2
  , Ad11.main1
  , Ad11.main2
  , Ad12.main1
  , Ad12.main2
  , Ad13.main1
  , Ad13.main2
  , Ad14.main1
  , Ad14.main2
  , Ad15.main1
  , Ad15.main2
  , Ad16.main1
  , Ad16.main2
  , Ad17.main1
  , Ad17.main2
  ]

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

      -- out <- openFile "BENCH.txt" WriteMode
      -- l <- latest
      -- mapM_ (time' out) $ drop l $ zip [0..] solutions
      -- hFlush out
      -- hClose out
    [readMaybe -> Just n, readMaybe -> Just m] -> solutions !! ((n - 1) * 2 + m - 1)
    _          -> putStrLn "Invalid problem number!"
