module Ad07 where

import Data.List
import Data.Function
import Data.Functor

readInput :: IO [Int]
readInput = readFile "input/07"
  <&> ("[" <>)
  <&> (<> "]")
  <&> read

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve f l@(x:_)
  = fuel l <$> [minimum l..maximum l]
  & foldl' min (fuel l x)
  where
    fuel :: [Int] -> Int -> Int
    fuel xs target = sum $ (f target) <$> xs
solve _ _ = error "Not enough input!"

dist :: Int -> Int -> Int
dist x y = abs (x - y)

solve1 :: [Int] -> Int
solve1 = solve dist

sumTo :: Int -> Int
sumTo n = n * (n + 1) `div` 2

sumToDist :: Int -> Int -> Int
sumToDist x y = sumTo (dist x y)

solve2 :: [Int] -> Int
solve2 = solve sumToDist

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
