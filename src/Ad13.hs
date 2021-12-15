{-# LANGUAGE LambdaCase #-}

module Ad13 where

import Control.Arrow
import Data.List
import Data.Bifunctor
import Data.Functor

type Pt = (Int, Int)
data Fold = X Int | Y Int
  deriving Show

readPt :: String -> Pt
readPt = bimap (read) (read . tail) . break (== ',')

readFold :: String -> Fold
readFold = dropWhile (not . (`elem` "xy"))
  >>> splitAt 2
  >>> \case
    ("x=", n) -> X $ read n
    ("y=", n) -> Y $ read n
    _ -> error "bad input"

readInput :: IO ([Pt], [Fold])
readInput = readFile "input/13"
  <&> lines
  <&> filter (/= "")
  <&> break ((== 'f') . head)
  <&> bimap (fmap readPt) (fmap readFold)

makeFold :: [Pt] -> Fold -> [Pt]
makeFold pts (X x') = fmap (\(x, y) -> (if x > x' then x'- (x - x') else x, y)) pts
makeFold pts (Y y') = fmap (\(x, y) -> (x, if y > y' then y' - (y - y') else y)) pts

solve1 :: [Pt] -> [Fold] -> Int
solve1 pts (f:_) = length $ nub $ makeFold pts f
solve1 _ _ = error "Expected at least one fold..."

solve2 :: [Pt] -> [Fold] -> [Pt]
solve2 pts fs = nub $ foldl' makeFold pts fs

main1 :: IO ()
main1 = readInput
  <&> uncurry solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> uncurry solve2
  >>= print
