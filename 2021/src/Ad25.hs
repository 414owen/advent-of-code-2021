{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ad25 (main) where

import Data.List
import Data.Function
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

data Dir = East | South
  deriving (Eq, Show)
type Pt = (Int, Int)
type Entities = Map Pt Dir

data Grid
  = Grid
  { width :: Int
  , height :: Int
  , entities :: Entities
  } deriving (Eq)

instance Show Grid where
  show (Grid {width, height, entities}) = intercalate "\n" $ [0..height - 1]
    <&> \y -> [0..width - 1]
    <&> \x -> case M.lookup (x, y) entities of
      Just South -> 'v'
      Just East -> '>'
      _ -> '.'

parseLine :: (Int, String) -> [(Pt, Dir)]
parseLine (y, line)
  = line
  & enumerate
  & concatMap (\(x, c) -> case c of
    '>' -> [((x, y), East)]
    'v' -> [((x, y), South)]
    '.'-> []
    _  -> error "bad cucumber")

readInput :: IO Grid
readInput = readFile "input/25"
  <&> lines
  <&> filter (/= "")
  <&> enumerate
  <&> \ls ->
    Grid { width = length $ snd $ head ls
         , height = length ls
         , entities = M.fromList $ concatMap parseLine ls
         }

next :: Grid -> (Pt, Dir) -> Pt
next Grid{height} ((x, y), South) = (x, (y + 1) `mod` height)
next Grid{width} ((x, y), East) = ((x + 1) `mod` width, y)

tick :: Grid -> Grid
tick g@Grid { entities } = g{ entities = entities'' }
  where

    moveEast es e@(_, East) = (case M.lookup (next g e) es of
      Just _ -> e
      Nothing -> (next g e, snd e))
    moveEast _ e = e

    entities' = entities
      & M.toList
      <&> moveEast entities
      & M.fromList

    moveSouth es e@(_, South) = (case M.lookup (next g e) es of
      Just _ -> e
      Nothing -> (next g e, snd e))
    moveSouth _ e = e

    entities'' = entities'
      & M.toList
      <&> moveSouth entities'
      & M.fromList

solve :: Int -> Grid -> Int
solve n grid = let next = tick grid
 in if next == grid
   then n
   else solve (n + 1) next

main :: IO ()
main = readInput >>= print . solve 1
