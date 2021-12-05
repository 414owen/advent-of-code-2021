{-# LANGUAGE ViewPatterns #-}

module Main where

import Text.Read
import System.Environment

import qualified Ad01
import qualified Ad02
import qualified Ad03
import qualified Ad04
import qualified Ad05

main :: IO ()
main = do
  args <- getArgs
  case args of
    [readMaybe -> Just day, readMaybe -> Just prob] -> case (day, prob) of
      (1, 1) -> Ad01.main1
      (1, 2) -> Ad01.main2
      (2, 1) -> Ad02.main1
      (2, 2) -> Ad02.main2
      (3, 1) -> Ad03.main1
      (3, 2) -> Ad03.main2
      (4, 1) -> Ad04.main1
      (4, 2) -> Ad04.main2
      (5, 1) -> Ad05.main1
      (5, 2) -> Ad05.main2
    _ -> putStrLn "Invalid problem number!"
