module Ad04 where

import Data.List
import Data.Function
import Data.Functor

readInput :: IO [String]
readInput = readFile "input/04"
  <&> lines
  <&> filter (/= "")

main1 :: IO ()
main1 = readInput
  >>= print

main2 :: IO ()
main2 = putStrLn "Not implemented"
