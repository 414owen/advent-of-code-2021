{-# LANGUAGE ViewPatterns #-}

module Main where

import Text.Read
import System.Environment

import qualified Ad01
import qualified Ad02
import qualified Ad03
import qualified Ad04
import qualified Ad05
import qualified Ad06
import qualified Ad07
import qualified Ad08
import qualified Ad09

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", "1"] -> Ad01.main1
    ["1", "2"] -> Ad01.main2
    ["2", "1"] -> Ad02.main1
    ["2", "2"] -> Ad02.main2
    ["3", "1"] -> Ad03.main1
    ["3", "2"] -> Ad03.main2
    ["4", "1"] -> Ad04.main1
    ["4", "2"] -> Ad04.main2
    ["5", "1"] -> Ad05.main1
    ["5", "2"] -> Ad05.main2
    ["6", "1"] -> Ad06.main1
    ["6", "2"] -> Ad06.main2
    ["7", "1"] -> Ad07.main1
    ["7", "2"] -> Ad07.main2
    ["8", "1"] -> Ad08.main1
    ["8", "2"] -> Ad08.main2
    ["9", "1"] -> Ad09.main1
    ["9", "2"] -> Ad09.main2
    _ -> putStrLn "Invalid problem number!"
