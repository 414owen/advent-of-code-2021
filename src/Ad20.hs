{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Ad20 (main1, main2) where

import Control.Category ((>>>))
import Data.List
import Data.Functor
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as S

type Pt = (Int, Int)

parseBoard :: [String] -> Set Pt
parseBoard board = S.fromList
    [ (x, y) | (y, row) <- zip [0..] board
             , (x, c) <- zip [0..] row
             , c == '#' ]

readInput :: IO (Vector Bool, Set Pt)
readInput = readFile "input/20"
  <&> lines
  <&> filter (/= "")
  <&> \case
    (rules : board) -> ( V.fromList $ fmap (== '#') rules
                       , parseBoard board)

getBounds :: Set Pt -> ((Int, Int), (Int, Int))
getBounds board = let (xs, ys) = unzip $ S.toList board
               in ((minimum xs - 1, minimum ys - 1), (maximum xs + 1, maximum ys + 1))

binToDec :: [Int] -> Int
binToDec = foldl' (\acc x -> acc * 2 + x) 0

tick :: Int -> Vector Bool -> Set Pt -> (Int, Set Pt)
tick n rule board = (n+1,) $ S.fromList $ do
     x <- [minx..maxx]
     y <- [miny..maxy]
     if rule V.! getInd (x, y) then pure (x, y) else []
  where
    minx, miny, maxx, maxy :: Int
    ((minx, miny), (maxx, maxy)) = getBounds board

    lit :: Pt -> Bool
    lit pt@(x, y) =
      (n `mod` 2 == 1 &&
        (x <= minx || y <= miny || x >= maxx || y >= maxy))
          || S.member pt board

    ptToBin :: Pt -> Int
    ptToBin pt = if lit pt then 1 else 0

    numToBin :: [Pt] -> [Int]
    numToBin = fmap ptToBin

    getNum :: [Pt] -> Int
    getNum = binToDec . numToBin

    getInd :: Pt -> Int
    getInd (x, y) = getNum $ do
      y' <- [y-1..y+1]
      x' <- [x-1..x+1]
      pure (x', y')

boards :: Vector Bool -> Set Pt -> [Set Pt]
boards rules board = snd <$> iterate tr (0, board)
  where tr (n, board) = tick n rules board

solve1 :: Vector Bool -> Set Pt -> Int
solve1 rules board = S.size $ boards rules board !! 2

main1 :: IO ()
main1 = readInput >>= print . uncurry solve1

solve2 :: Vector Bool -> Set Pt -> Int
solve2 rules board = S.size $ boards rules board !! 50

main2 :: IO ()
main2 = readInput >>= print . uncurry solve2
