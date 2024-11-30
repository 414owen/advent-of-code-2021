module Ad03 where

import Data.Char (digitToInt)
import Data.List
import Data.Functor

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

readInput :: IO [String]
readInput = readFile "input/03"
  <&> lines
  <&> filter (/= "")

num :: Eq a => a -> [a] -> Int
num x xs = length $ filter (== x) xs

gamma :: [String] -> String
gamma [] = []
gamma (cs:xs)
  | num '0' cs > num '1' cs = '0' : gamma xs
  | otherwise = '1' : gamma xs

epsilon :: [String] -> String
epsilon [] = []
epsilon (cs:xs)
  | num '0' cs <= num '1' cs = '0' : epsilon xs
  | otherwise = '1' : epsilon xs

ox :: Int -> [String] -> String
ox _ [x] = x
ox n xs = let
  t = transpose xs
  c = gamma t !! n
  ys = filter ((== c) . (!! n)) xs
  in ox (n + 1) ys

co2 :: Int -> [String] -> String
co2 _ [x] = x
co2 n xs = let
  t = transpose xs
  c = epsilon t !! n
  ys = filter ((== c) . (!! n)) xs
  in co2 (n + 1) ys

solve2 :: [String] -> Int
solve2 l =
  let
    o = ox 0 l
    c = co2 0 l
  in toDec o * toDec c

solve1 :: [String] -> Int
solve1 l =
  let
    g = gamma l
    e = epsilon l
  in toDec g * toDec e

main1 :: IO ()
main1 = readInput >>= print . solve1 . transpose

main2 :: IO ()
main2 = readInput >>= print . solve2
