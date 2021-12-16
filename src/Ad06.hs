module Ad06 where

import Control.Category ((>>>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Function
import Data.Functor

count :: Ord a => [a] -> Map a Int
count [] = mempty
count (x:xs)
  = M.insert x (length $ filter (== x) (x:xs))
  $ count (filter (/= x) xs)

readInput :: IO (Map Int Int)
readInput = readFile "input/06"
  <&> (\s -> "[" <> s <> "]")
  <&> read
  <&> count

addSert :: Ord k => k -> Int -> Map k Int -> Map k Int
addSert k v = M.alter f k
  where
    f Nothing = Just v
    f (Just a) = Just $ v + a

iter :: Int -> Map Int Int -> Map Int Int
iter 0 m = m
iter n m = m
  & M.assocs
  & foldl' iter' mempty
  & iter (n - 1)
  where
    iter' :: Map Int Int -> (Int, Int) -> Map Int Int
    iter' m' (0, v) = addSert 8 v $ addSert 6 v m'
    iter' m' (n', v) = addSert (n' - 1) v m'

solveN :: Int -> Map Int Int -> Int
solveN n = iter n
  >>> M.elems
  >>> sum

solve1 :: Map Int Int -> Int
solve1 = solveN 80

solve2 :: Map Int Int -> Int
solve2 = solveN 256

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
