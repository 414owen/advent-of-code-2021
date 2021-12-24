{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Ad23 (main1, main2) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Functor
import Data.Foldable

data Pod = A | B | C | D
  deriving (Show, Eq, Ord)
type Pt = (Int, Int)
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

readInput :: String -> IO Input
readInput path = readFile ("input/" <> path)
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

getMoves :: Int -> Map Pt Pod -> Pt -> Pod -> [Pt]
getMoves maxY m (x, y) podType =
  let dx = destX podType
      inHall = y == 1
      dps = (dx,) <$> [2..maxY]
  in
  if | x == dx && all (Just podType ==) (flip M.lookup m . (x,) <$> [y+1..maxY]) -> []
     | inHall
         && all (isEmpty m) ((,1) <$> to x dx)
         && all (\p -> not (M.member p m) || M.lookup p m == Just podType)
             dps -> [maximum $ filter (isEmpty m) dps]
     | not inHall
         && all (isEmpty m) ((x,) <$> [2..y-1]) ->
           filter (not . isDest . fst) $
             takeWhile (isEmpty m) ((,1) <$> to x 1) <>
             takeWhile (isEmpty m) ((,1) <$> to x 11)
     | True -> []

getCost :: Pod -> Int
getCost A = 1
getCost B = 10
getCost C = 100
getCost D = 1000

manhattan :: Pt -> Pt -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type DP = Map (Map Pt Pod) Int

search :: Int -> Int -> Int -> Int -> Map Pt Pod -> DP -> (Int, DP)
search _ _ best energy _ dp | energy >= best = (best, dp)
search _ 0 _ energy _ dp = (energy, dp)
search maxY todo best energy pods dp =
  let isWorse = case M.lookup pods dp of
        Just e | e <= energy -> True
        _ -> False
  in if isWorse
    then (best, dp)
    else foldl' rec (best, M.insert pods energy dp) $ do
      (pt, pod) <- M.toList pods
      pt2 <- getMoves maxY pods pt pod
      [(pt, pt2, pod)]
  where
    rec :: (Int, DP) -> (Pt, Pt, Pod) -> (Int, DP)
    rec (best', dp') (from, to', pod)
      = first (min best') $ search maxY
          (if snd to' /= 1 then todo - 1 else todo) best'
          (energy + manhattan from to' * getCost pod)
          (M.insert to' pod $ M.delete from pods)
          dp'

solve :: Int -> Input -> Int
solve maxY pods =
  let podSet = M.fromList pods
      cols = catMaybes <$> [[M.lookup (x, y) podSet | y <- reverse [2..maxY]] | x <- [3, 5, 7, 9]]
      corrects = sum $ length <$> zipWith (\p ps -> takeWhile (== p) ps) [A, B, C, D] cols
      todo = M.size podSet - corrects
  in fst $ search maxY todo maxBound 0 podSet mempty

main1 :: IO ()
main1 = readInput "23" >>= print . solve 3

main2 :: IO ()
main2 = readInput "23Part2" >>= print . solve 5
