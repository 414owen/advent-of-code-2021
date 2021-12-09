{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Ad09 where

import Control.Category ((>>>))
import Control.Monad
import Data.List
import Data.Function
import Data.Functor
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.RWS.Strict
import Debug.Trace

readInput :: IO [[Int]]
readInput = readFile "input/09"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap (\a -> read [a]))

rowLow :: [Int] -> [Bool]
rowLow [x1, x2, x3] = [x1 < x2, x2 < x1 && x2 < x3, x3 < x2]
rowLow (x1 : x2 : x3 : xs) = (x1 < x2) : (x2 < x1 && x2 < x3) : tail (rowLow (x2 : x3 : xs))

horLow :: [[Int]] -> [[Bool]]
horLow m = fmap rowLow m

getLows :: [[Int]] -> [[Bool]]
getLows m =
  let
    t = transpose m
    b1 = horLow m
    b2 = transpose $ horLow t
  in zipWith (zipWith (&&)) b1 b2

pts :: Int -> Bool -> Int
pts _ False = 0
pts n True = n + 1

solve1 :: [[Int]] -> Int
solve1 xs = getLows xs
  & zipWith (zipWith pts) xs
  & concat
  & sum

zipInds :: [[a]] -> [[((Int, Int), a)]]
zipInds = zipWith (\y r -> zipWith (\x a -> ((x, y), a)) [0..] r) [0..]

type Grid = Vector (Vector Int)
type Pt = (Int, Int)
type Search a = RWS Grid () (Set Pt) a

search :: Pt -> Search Int
search pt@(x, y) = do
  grid <- ask
  seen <- get
  if | x < 0                       -> pure 0
     | y < 0                       -> pure 0
     | y >= V.length grid          -> pure 0
     | x >= V.length (grid V.! 0)  -> pure 0
     | (grid V.! y) V.! x == 9     -> pure 0
     | S.member pt seen            -> pure 0
     | otherwise -> do
         modify (S.insert pt)
         a <- search (x + 1, y)
         b <- search (x - 1, y)
         c <- search (x, y + 1)
         d <- search (x, y - 1)
         pure (1 + a + b + c + d)

getBasins :: [[Int]] -> [Int]
getBasins xs =
  let
    ls = getLows xs
    inds = zipInds ls & concat & filter snd & fmap fst
    grid = V.fromList $ V.fromList <$> xs
  in
  fst $ evalRWS (forM inds search) grid mempty

solve2 :: [[Int]] -> Int
solve2 = getBasins
  >>> sort
  >>> reverse
  >>> \case
    (x : y : z : _) -> x * y * z


main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
