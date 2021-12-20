{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ad19 (main1, main2) where

import Control.Applicative
import Control.Category ((>>>))
import Data.List
import Data.Functor
import Data.Maybe
import Data.List.Split
import Control.Monad.State.Strict

type Pt = (Int, Int, Int)

parsePoint :: String -> Pt
parsePoint = splitOn ","
  >>> fmap read
  >>> \case
    [a, b, c] -> (a, b, c)
    _ -> error "Bad point!"

readInput :: IO [[Pt]]
readInput = readFile "input/19"
  <&> lines
  <&> filter (/= "")
  <&> splitWhen ("---" `isPrefixOf`)
  <&> filter (/= [])
  <&> fmap (fmap parsePoint)

data Direction = Forward | Up | Down | DRight | DLeft | Backward
  deriving (Bounded, Enum, Show)

data Rotation = RZero | R90 | R180 | R270
  deriving (Bounded, Enum, Show)

type Orientation = (Direction, Rotation)

type Translation = Pt

type Transform = (Translation, Orientation)

-- Scanner zero has transform ((0,0,0), Forward, RZero)
-- x: Left - Right
-- y: Forward - Backward

allOrientations :: [Orientation]
allOrientations = do
  dir <- [minBound..maxBound]
  rot <- [minBound..maxBound]
  [(dir, rot)]

type M = State ([(Pt, [Pt])], [[Pt]])

direct :: Direction -> Pt -> Pt
direct Forward pt = pt
-- for y = up -z
-- for z = up y
direct Up (x, y, z) = (x, negate z, y)
-- for y = down z
-- for z = down -y
direct Down (x, y, z) = (x, z, negate y)
-- for x = right y
-- for y = right -x
direct DRight (x, y, z) = (y, negate x, z)
-- for x = left -y
-- for y = left x
direct DLeft (x, y, z) = (negate y, x, z)
-- for z = back -z
-- for y = back -y
direct Backward (x, y, z) = (x, negate y, negate z)

rotate :: Rotation -> Pt -> Pt
rotate RZero pt = pt
rotate R90 (x, y, z) = (negate z, y, x)
rotate R180 (x, y, z) = (negate x, y, negate z)
rotate R270 (x, y, z) = (z, y, negate x)

reorient :: Orientation -> Pt -> Pt
reorient (direction, rotation) pt =
  rotate rotation $ direct direction pt

translate :: Translation -> Pt -> Pt
translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

reinterpret :: Transform -> Pt -> Pt
reinterpret (translation, orientation) pt
  = translate translation $ reorient orientation pt

reinterpretAll :: Transform -> [Pt] -> [Pt]
reinterpretAll (transform) = fmap (reinterpret transform)

manhattan :: Pt -> Pt -> Int
manhattan (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

dists :: [Pt] -> [Int]
dists [] = []
dists (x : xs) = fmap (manhattan x) xs <> dists xs

listersect :: Ord a => [a] -> [a] -> [a]
listersect xs ys = xs \\ (xs \\ ys)

distsMatch :: [Pt] -> [Pt] -> Bool
distsMatch xs ys = length (listersect (dists xs) (dists ys)) >= 12

trycanoncalize :: [Pt] -> [Pt] -> Maybe ([Pt], Pt, [Pt])
trycanoncalize can rel = listToMaybe $ do
  guard (distsMatch can rel)
  (x, y, z) <- can
  p2 <- rel
  orientation <- allOrientations
  let (x', y', z') = reorient orientation p2
  let displacement  = (x - x', y - y', z - z')
  let transform = ((displacement, orientation) :: Transform)
  let ys' = reinterpretAll transform rel
  case listersect can ys' of
    xs' | length xs' >= 12 -> pure $ (ys', displacement, rel)
        | otherwise -> []

canonicalizeAll :: M [(Pt, [Pt])]
canonicalizeAll = do
  (cans, rels) <- get
  case rels of
    [] -> pure cans
    _ -> do
      let (newcan, pt, oldrel) = rec (fmap snd $ cans) rels
      modify $ \case
        (cans', rels') -> ((pt, newcan) : cans', filter (/= oldrel) rels')
      case rels of
        [_] -> fst <$> get
        _ -> canonicalizeAll

  where
    rec :: [[Pt]] -> [[Pt]] -> ([Pt], Pt, [Pt])
    rec cans rels = case rec' cans rels of
      Nothing -> error "out of canonicalized"
      Just a -> a

    rec' :: [[Pt]] -> [[Pt]] -> Maybe ([Pt], Pt, [Pt])
    rec' [] _ = Nothing
    rec' _ [] = Nothing
    rec' (can : cans) (rel : rels) =
      trycanoncalize can rel
      <|> rec' (can : cans) rels
      <|> rec' cans (rel : rels)

solve :: [[Pt]] -> [(Pt, [Pt])]
solve [] = error "Bad input"
solve (x : xs) = fst $ flip execState ([((0,0,0), x)], xs) canonicalizeAll

solve1 :: [[Pt]] -> Int
solve1 scanners =
  length $ nub $ concat $ fmap snd $ solve scanners

solve2 :: [[Pt]] -> Int
solve2 scanners = maximum $ do
  let xs' = nub $ fmap fst $ solve scanners
  a <- xs'
  b <- xs'
  [manhattan a b]

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
