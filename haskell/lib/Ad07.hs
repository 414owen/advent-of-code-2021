module Ad07 where

import Control.Monad
import Data.List
import Data.Function
import Data.Functor

readInput :: IO [Int]
readInput = readFile "input/07"
  <&> ("[" <>)
  <&> (<> "]")
  <&> read


solve1 :: [Int] -> Int
solve1 xs = foldl' min (dist mi) $ dist <$> [mi..ma]
  where
    mi = minimum xs
    ma = maximum xs

    dist :: Int -> Int
    dist = dist' 0 xs

    dist' :: Int -> [Int] -> Int -> Int
    dist' n [] _ = n
    dist' n (x:xs) target =
      dist' (n + abs (x - target)) xs target

sumto :: Int -> Int
sumto n = n * (n + 1) `div` 2

solve2 :: [Int] -> Int
solve2 xs = foldl' min (dist mi) $ dist <$> [mi..ma]
  where
    mi = minimum xs
    ma = maximum xs

    dist :: Int -> Int
    dist = dist' 0 xs

    dist' :: Int -> [Int] -> Int -> Int
    dist' n [] _ = n
    dist' n (x:xs) target =
      dist' (n + sumto (abs (x - target))) xs target


main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
