module Ad01 where

import Data.Functor
import Control.Category hiding ((.))
import Data.Function

solve1 :: [Int] -> Int
solve1 xs = zip xs (tail xs)
  & increases

increases :: [(Int, Int)] -> Int
increases = fmap (\(a, v) -> if v > a then 1 else 0)
  >>> sum

windows' :: Int -> [a] -> [[a]]
windows' _ [] = []
windows' n xs = take n xs : windows n (tail xs)

windows :: Int -> [a] -> [[a]]
windows n xs = windows' n xs
  & filter ((== n) . length)

readInput :: IO [Int]
readInput = readFile "input/01"
  <&> lines
  <&> filter (/= "")
  <&> fmap read

solve2 :: [Int] -> Int
solve2 xs = let ys = sum <$> windows 3 xs in
  zip ys (tail ys)
  & increases

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
