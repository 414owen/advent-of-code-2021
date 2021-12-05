{-# LANGUAGE ViewPatterns #-}

module Ad05 where

import Control.Category ((>>>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Function
import Data.Functor

type Pt = (Int, Int)
type Line = (Pt, Pt)

readPt :: String -> Pt
readPt s = read $ "(" <> s <> ")"

readLine :: String -> Line
readLine s =
  let p1 = takeWhile (/= ' ') s & readPt
      p2 = dropWhile (/= ' ') s & dropWhile (not . isDigit) & readPt
  in (p1, p2)

readInput :: IO [Line]
readInput = readFile "input/05"
  <&> lines
  <&> filter (/= "")
  <&> fmap readLine

approach' :: Int -> Int -> Int
approach' a b
  | a == b = a
  | a < b = a + 1
  | otherwise = a - 1

approach :: Line -> Line
approach ((x1, y1), (x2, y2))
  = ((approach' x1 x2, approach' y1 y2), (x2, y2))

addPt :: Map Pt Int -> Pt -> Map Pt Int
addPt m p = M.alter f p m
  where
    f (Nothing) = Just 1
    f (Just a) = Just $ a + 1

addLine :: Map Pt Int -> Line -> Map Pt Int
addLine m l@(p1@(x1, y1), p2@(x2, y2))
  | p1 == p2 = addPt m (x1, y1)
  | otherwise = addLine (addPt m p1) (approach l)

solve :: [Line] -> Int
solve = foldl' addLine mempty
  >>> M.elems
  >>> filter (> 1)
  >>> length

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

main1 :: IO ()
main1 = readInput
  <&> filter isStraight
  <&> solve
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve
  >>= print

