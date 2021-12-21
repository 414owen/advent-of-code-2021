{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ad19 (main1, main2) where

import Control.Arrow
import Data.List
import Data.Functor
import Data.Maybe
import Data.List.Split
import Control.Monad.RWS.Strict
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

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
  <&> fmap (sort . fmap parsePoint)

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

transform :: Transform -> Pt -> Pt
transform (translation, orientation) pt
  = translate translation $ reorient orientation pt

transformAll :: Transform -> [Pt] -> [Pt]
transformAll transformation = fmap (transform transformation)

manhattan :: Pt -> Pt -> Int
manhattan (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

distHash :: Pt -> Pt -> Pt
distHash (x, y, z) (x1, y1, z1)
  = case sort $ abs <$> [x1 - x, y1 - y, z1 - z] of
      [a, b, c] -> (a, b, c)
      _ -> error "impossible"

getDists :: [Pt] -> [Pt]
getDists [] = []
getDists (x : xs) = fmap (distHash x) xs <> getDists xs

listersectSorted :: Ord a => [a] -> [a] -> [a]
listersectSorted [] _ = []
listersectSorted _ [] = []
listersectSorted xs'@(x : xs) ys'@(y : ys)
  | x < y = listersectSorted xs ys'
  | x > y = listersectSorted xs' ys
  | otherwise = x : listersectSorted xs ys

newtype Scanner = Scanner { id :: Int }
  deriving (Show, Eq, Ord)

data State =
  State
  { rels :: [Scanner]
  , cans :: [Scanner]
  , canons :: Map Scanner (Pt, [Pt])
  }

type M = RWS (Map Scanner (Set Scanner), Map Scanner [Pt]) () State

listToMaybe' :: [a] -> Maybe a
listToMaybe' (x : _) = Just x
listToMaybe' _ = Nothing


trycanoncalize :: Scanner -> Scanner -> M (Maybe (Scanner, [Pt], Pt))
trycanoncalize scanId relId = do
  (compatMap, relMap) <- ask
  if S.member relId $ fromJust $ M.lookup scanId compatMap
    then do
      (_, can) <- fromJust . M.lookup scanId . canons <$> get
      let rel = fromJust $ M.lookup relId relMap
      pure $ listToMaybe' $ do
        p2 <- rel
        orientation <- allOrientations
        let (x', y', z') = reorient orientation p2
        (x, y, z) <- can
        let displacement  = (x - x', y - y', z - z')
        let transformation = (displacement, orientation)
        let ys' = sort $ transformAll transformation rel
        case listersectSorted can ys' of
          (_ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _) ->
            [(relId, ys', displacement)]
          _ -> []
    else pure Nothing

canonicalizeAll :: M [(Pt, [Pt])]
canonicalizeAll = do
  State { rels, cans, canons } <- get
  if null rels
    then pure $ M.elems canons
    else do
      (scanId, canPts, scanLoc) <- rec cans rels
      modify $ \s ->
          s{ rels = filter (/= scanId) rels
           , cans = scanId : cans
           , canons = M.insert scanId (scanLoc, canPts) canons
           }
      canonicalizeAll
  where
    rec :: [Scanner] -> [Scanner] -> M (Scanner, [Pt], Pt)
    rec cans rels = rec' cans rels <&> \case
      Nothing -> error "out of canonicalized"
      Just a -> a

    rec' :: [Scanner] -> [Scanner] -> M (Maybe (Scanner, [Pt], Pt))
    rec' [] _ = pure Nothing
    rec' _ [] = pure Nothing
    rec' (can : cans) (rel : rels) =
      trycanoncalize can rel >>= \case
        Nothing -> do
          b <- rec' (can : cans) rels
          case b of
            Nothing -> rec' cans (rel : rels)
            a -> pure a
        a -> pure a

getCompatible :: Map Scanner [Pt] -> [Scanner] -> Map Scanner (Set Scanner)
getCompatible m scs = M.fromListWith S.union $ do
  sca <- scs
  scb <- scs
  guard (sca /= scb)
  let Just sa = M.lookup sca m
  let Just sb = M.lookup scb m
  [(sca, S.singleton scb) | length (listersectSorted sa sb) >= 12]

solve :: [[Pt]] -> [(Pt, [Pt])]
solve (fmap (first Scanner) . zip [0..] -> xs'@(x : xs)) =
  let dists = M.fromList $ second (sort . getDists) <$> xs'
      compat = getCompatible dists $ fmap fst xs'
      initial = State
              { rels = fst <$> xs
              , cans = [Scanner 0]
              , canons = M.fromList [(Scanner 0, ((0,0,0), snd x))]
              }
      (ans, _) = evalRWS canonicalizeAll (compat, M.fromList xs) initial
  in ans
solve _ = error "Bad input"

solve1 :: [[Pt]] -> Int
solve1 scanners =
  length $ nub $ concatMap snd $ solve scanners

solve2 :: [[Pt]] -> Int
solve2 scanners = maximum $ do
  let xs' = nub $ fst <$> solve scanners
  a <- xs'
  b <- xs'
  [manhattan a b]

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
