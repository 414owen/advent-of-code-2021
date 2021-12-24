{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Ad23Part2 (main) where

import Data.Tuple
import Control.Applicative
import Control.Arrow
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Functor
import Data.Function
import Data.Foldable
import Debug.Trace

data Pod = A | B | C | D
  deriving (Show, Eq, Ord)
type Pt = (Int, Int)
type Energy = Int
type Input = [(Pt, Pod)]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

toPod :: Char -> Maybe Pod
toPod 'A' = Just A
toPod 'B' = Just B
toPod 'C' = Just C
toPod 'D' = Just D
toPod _ = Nothing

toPods :: (Int, String) -> [(Pt, Pod)]
toPods (y, line) = do
  (x, cell) <- enumerate line
  maybeToList $ ((x, y),) <$> toPod cell

readInput :: IO Input
readInput = readFile "input/23Part2"
  <&> lines
  <&> filter (/= "")
  <&> concatMap toPods . enumerate

destX :: Pod -> Int
destX A = 3
destX B = 5
destX C = 7
destX D = 9

isDest :: Int -> Bool
isDest 3 = True
isDest 5 = True
isDest 7 = True
isDest 9 = True
isDest _ = False

isEmpty :: Map Pt Pod -> Pt -> Bool
isEmpty m pt = not $ M.member pt m

data Dir = Left | Right | Up | Down
  deriving (Show, Eq)

-- doesn't include first pt
to :: Int -> Int -> [Int]
to x1 x2 | x1 < x2 = [x1 + 1 .. x2]
         | otherwise = reverse [x2 .. x1 - 1]

maxY :: Int
maxY = 5

roomYs :: [Int]
roomYs = [2..maxY]

getMoves :: Map Pt Pod -> Pt -> Pod -> [Pt]
getMoves m (x, y) podType =
  let dx = destX podType
      inHall = y == 1
      dps = (dx,) <$> roomYs
  in
  if | x == dx && all (Just podType ==) (flip M.lookup m . (x,) <$> [y+1..5]) -> []
     | inHall
         && all (isEmpty m) ((,1) <$> (to x dx))
         && all (\p -> not (M.member p m) || M.lookup p m == Just podType)
             dps -> [maximum $ filter (isEmpty m) dps]
     | not inHall
         && all (isEmpty m) ((x,) <$> [2..y-1]) ->
           filter (not . isDest . fst) $
             (takeWhile (isEmpty m) $ (,1) <$> to x 1) <>
             (takeWhile (isEmpty m) $ (,1) <$> to x 11)
     | True -> []

getCost :: Pod -> Int
getCost A = 1
getCost B = 10
getCost C = 100
getCost D = 1000

manhattan :: Pt -> Pt -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type DP = Map (Map Pt Pod) Int

search :: Int -> Int -> Energy -> Map Pt Pod -> DP -> (Int, DP)
search _ best energy _ dp | energy >= best = (best, dp)
search 0 _ energy pods dp = (energy, dp)
search todo best energy pods dp =
  let isWorse = case M.lookup pods dp of
        Just e | e <= energy -> True
        _ -> False
  in if isWorse
    then (best, dp)
    else foldl' rec (best, M.insert pods energy dp) $ do
      (pt, pod) <- M.toList pods
      pt2 <- getMoves pods pt pod
      [(pt, pt2, pod)]
  where
    rec :: (Int, DP) -> (Pt, Pt, Pod) -> (Int, DP)
    rec (best', dp') (from, to, pod)
      = first (min best') $ search
          (if snd to /= 1 then todo - 1 else todo) best'
          (energy + manhattan from to * getCost pod)
          (M.insert to pod $ M.delete from pods)
          dp'

solve :: Input -> Int
solve pods =
  let podSet = M.fromList pods
      cols = catMaybes <$> [[M.lookup (x, y) podSet | y <- reverse [2..maxY]] | x <- [3, 5, 7, 9]]
      corrects = sum $ length <$> zipWith (\p ps -> takeWhile (== p) ps) [A, B, C, D] cols
      todo = M.size podSet - corrects
  in fst $ search todo maxBound 0 podSet mempty

main :: IO ()
main = readInput >>= print . solve
