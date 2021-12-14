{-# LANGUAGE TupleSections #-}

module Ad14 where

import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Char
import Data.Functor

type Path = ((Char, Char), Char)
type Input = (String, [Path])

readPath :: String -> Path
readPath (c1 : c2 : ' ' : '-' : '>' : ' ' : c3 : []) = ((c1, c2), c3)

windows' :: Int -> [a] -> [[a]]
windows' n [] = []
windows' n xs = take n xs : windows n (tail xs)

windows :: Int -> [a] -> [[a]]
windows n xs = windows' n xs
  & filter ((== n) . length)

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

readInput :: IO Input
readInput = readFile "input/14"
  <&> lines
  <&> filter (/= "")
  <&> splitAt 1
  <&> first head
  <&> second (fmap readPath)

type Occ a = (a, Int)

addOcc :: Eq a => Occ a -> [Occ a] -> [Occ a]
addOcc (s, o) [] = [(s, o)]
addOcc (s, o) ((s1, o1) : xs)
  | s == s1 = (s, o1 + o) : xs
  | otherwise = (s1, o1) : addOcc (s, o) xs

step :: [Path] -> [Occ (Char, Char)] -> [Occ (Char, Char)]
step paths ((x1@(a, b), c) : xs) =
  let out = fromJust $ lookup x1 paths
      re = step paths xs
  in addOcc ((a, out), c) $ addOcc ((out, b), c) re
step _ [] = []

chains :: [Path] -> [Occ (Char, Char)] -> [[Occ (Char, Char)]]
chains paths start = start : chains paths (step paths start)

tupOccs :: String -> [((Char, Char), Int)]
tupOccs xs =
  let w = (\[a, b] -> (a, b)) <$> windows 2 xs
      u = nub w
  in u <&> \tup -> (tup, count tup w)

solve :: Int -> Input -> Int
solve iters (start, paths) =
  let occs = tupOccs start
      end = chains paths occs !! iters
      counts = addOcc (head start, 1)
        $ addOcc (last start, 1)
        $ foldl' (flip addOcc) [] ((end >>= \((c1, c2), c) -> (,c) <$> [c1, c2]) :: [Occ Char])
      ma = maximum $ snd <$> counts
      mi = minimum $ snd <$> counts
  in (ma `div` 2) - (mi `div` 2)

solve1 :: Input -> Int
solve1 = solve 10

solve2 :: Input -> Int
solve2 = solve 40

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
