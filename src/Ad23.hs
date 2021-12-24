{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Ad23 (main) where

import Data.Tuple
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
data Cell = Wall | Empty
  deriving (Show, Eq, Ord)
type Pt = (Int, Int)
type Energy = Int
type GridState = (Energy, Map Pt Pod)
type Grid = Vector (Vector Cell)
type Input = (Grid, [(Pt, Pod)])

toCell :: Char -> Cell
toCell = \case
  '#' -> Wall
  _ -> Empty

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

toPod :: Char -> Maybe Pod
toPod = \case
  'A' -> Just A
  'B' -> Just B
  'C' -> Just C
  'D' -> Just D
  _ -> Nothing

toPods :: (Int, String) -> [(Pt, Pod)]
toPods (y, line) = do
  (x, cell) <- enumerate line
  maybeToList $ ((x, y),) <$> toPod cell

readInput :: IO Input
readInput = readFile "input/23"
  <&> lines
  <&> filter (/= "")
  <&> (V.fromList . fmap V.fromList . fmap (fmap toCell) &&& concatMap toPods . enumerate)

type M = State (Map (Map Pt Pod) Int)

final :: Map Pt Pod
final =
  M.fromList $ swap <$>
  [ (A, (3, 2))
  , (A, (3, 3))
  , (B, (5, 2))
  , (B, (5, 3))
  , (C, (7, 2))
  , (C, (7, 3))
  , (D, (9, 2))
  , (D, (9, 3))
  ]

destX :: Pod -> Int
destX = \case
  A -> 3
  B -> 5
  C -> 7
  D -> 9

dests :: [Int]
dests = [3, 5, 7, 9]

toPoses :: Map Pt Pod -> [(Pod, Pt)]
toPoses = M.toList >>> fmap swap

inHall :: Pt -> Bool
inHall (x, y) = y == 1

podPts :: GridState -> [Pt]
podPts (_, pods) = M.keys pods

isEmpty :: GridState -> Pt -> Bool
isEmpty (e, m) pt = not $ M.member pt m

data Dir = Left | Right | Up | Down
  deriving (Show, Eq)

to :: Int -> Int -> [Int]
to x1 x2 = filter (/= x1) [min x1 x2 .. max x1 x2]

getMoves :: GridState -> Pt -> Pod -> [Pt]
getMoves state@(e, m) pt@(x, y) podType =
  let dx = destX podType in

  if | x == dx && y == 3 -> []
     | inHall pt
         && all (isEmpty state) ((,y) <$> (to x dx))
         && isEmpty state (dx, 2) ->
           if | isEmpty state (dx, 3) -> [(dx, 3)]
              | (Just podType == M.lookup (dx, 3) m) -> [(dx, 2)]
              | otherwise -> []
     | y == 3 && M.member (x, 2) m -> []
     | not (inHall pt) ->
         fmap (,1) $ filter (not . (`elem` dests)) $
         ((takeWhile (not . flip M.member m . (,1)) $ to x 1) :: [Int]) <>
         ((takeWhile (not . flip M.member m . (,1)) $ to x 11) :: [Int])
     | True -> []

getCost :: Pod -> Int
getCost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

manhattan :: Pt -> Pt -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

search :: GridState -> M Int
search state@(energy, pods) = do
  if pods == final
    then pure (traceShowId energy)
    else do
      stateSet <- get
      case M.lookup pods stateSet of
        Just e | e <= energy -> pure maxBound
        _ -> do
          modify $ M.insert pods energy
          fmap (minimum . (maxBound:)) $ forM (M.toList pods) $ \(pt, pod) -> do
            let moves = getMoves state pt pod
            let pods' = M.delete pt pods
            fmap (minimum . (maxBound:)) $ forM moves $ \pt2 -> do
              -- let cost = traceShow (pod, energy, pt, pt2) $ getCost pod * manhattan pt pt2
              let cost = getCost pod * manhattan pt pt2
              let pods'' = M.insert pt2 pod pods'
              search (energy + cost, pods'')

solve1 :: Input -> Int
solve1 (grid, pods) =
  let podSet = M.fromList pods
      stateSet = mempty
  in evalState (search (0, podSet)) stateSet

main :: IO ()
main = readInput >>= print . solve1
