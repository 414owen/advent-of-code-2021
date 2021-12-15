{-# LANGUAGE MultiWayIf #-}

module Ad15 where

import Data.List
import Data.Maybe
import Data.Function
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.RWS.Strict

type Input = (Vector (Vector Int))

readInput :: IO Input
readInput = readFile "input/15"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap (read . (:[])))
  <&> fmap V.fromList
  <&> V.fromList

type M = RWS Input () (Map (Int, Int) Int)

search :: Int -> Int -> Int -> M ()
search x y risk = do
  grid <- ask
  state <- get
  let h = V.length grid
  let w = V.length (V.head grid)
  if | x < 0                       -> pure ()
     | y < 0                       -> pure ()
     | M.member (w-1, h-1) state && Just risk >= M.lookup (w-1, h-1) state -> pure ()
     | y >= V.length grid          -> pure ()
     | x >= V.length (V.head grid) -> pure ()
     | otherwise -> do
         let myrisk = risk + grid V.! y V.! x
         if | M.member (x, y) state && Just myrisk >= M.lookup (x, y) state -> pure ()
            | otherwise -> do
                modify $ M.insert (x, y) myrisk
                search (x + 1) y myrisk
                search x (y + 1) myrisk
                search (x - 1) y myrisk
                search x (y - 1) myrisk

solve :: Input -> Int
solve grid =
  let h = V.length grid
      w = V.length (V.head grid)
  in execRWS (search 0 0 (0 - (grid V.! 0 V.! 0))) grid mempty
    & fst
    & M.lookup (w-1, h-1)
    & fromJust

main1 :: IO ()
main1 = readInput
  <&> solve
  >>= print

add :: (Functor f, Functor g, Integral a) => f (g a) -> a -> f (g a)
add grid n = fmap ((+ 1) . (`mod` 9) . (\a -> a - 1) . (+ n)) <$> grid

construct :: Input -> Input
construct grid
  = grid
  & V.toList
  & fmap V.toList
  & \grid -> add grid <$> [0..4]
    & concat
    & transpose
    & (\g -> add g <$> [0..4])
    & concat
    & transpose
    & fmap V.fromList
    & V.fromList


main2 :: IO ()
main2 = readInput
  <&> construct
  <&> solve
  >>= print
  -- <&> fmap V.toList
  -- <&> V.toList
  -- <&> fmap (fmap show)
  -- <&> fmap (concat)
  -- >>= mapM_ putStrLn
