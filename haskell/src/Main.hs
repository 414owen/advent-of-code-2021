{-# LANGUAGE ViewPatterns #-}

module Main where

import Text.Read
import System.Environment

import qualified Ad01

main :: IO ()
main = do
  args <- getArgs
  case args of
    [readMaybe -> Just day, readMaybe -> Just prob] -> case (day, prob) of
      (1, 1) -> Ad01.main1
      (1, 2) -> Ad01.main2
    _ -> putStrLn "Invalid problem number!"
