{-# LANGUAGE MultiWayIf #-}

module Ad12 where

import Control.Arrow
import Control.Monad
import Data.Tuple
import Data.Char
import Data.Functor

type Adj = [(String, String)]

readInput :: IO Adj
readInput = readFile "input/12"
  <&> lines
  <&> filter (/= "")
  <&> fmap (second tail . break (== '-'))

next :: String -> Adj -> [String]
next node adj = snd <$> filter ((== node) . fst) adj

solve1 :: String -> [String] -> Adj -> Int
solve1 "end" visited adj = 1
solve1 node@(x:xs) visited adj
  | isLower x && node `elem` visited = 0
  | otherwise =
      let  nv = if isLower x then node : visited else visited
      in sum $ flip fmap (next node adj) $ \n -> solve1 n nv adj

solve2 :: String -> [String] -> Bool -> Adj -> Int
solve2 "end" visited _ _ = 1
solve2 "start" (_:_) _ _ = 0
solve2 node@(x:xs) visited twice adj
  | twice && isLower x && node `elem` visited = 0
  | otherwise =
      let nv = (if isLower x then (node :) else id) visited
          ntwice = twice || isLower x && node `elem` visited
      in sum $ flip fmap (next node adj) $ \n -> solve2 n nv ntwice adj

preprocess :: [(String, String)] -> [(String, String)]
preprocess = ap (<>) (fmap swap)

main1 :: IO ()
main1 = readInput
  <&> preprocess
  <&> solve1 "start" []
  >>= print

main2 :: IO ()
main2 = readInput
  <&> preprocess
  <&> solve2 "start" [] False
  >>= print
