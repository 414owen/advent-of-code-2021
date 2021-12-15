{-# LANGUAGE MultiWayIf #-}

module Ad15 (main1, main2) where

import Data.List
import Data.Function
import Data.Functor
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Control.Monad.Reader
import Control.Monad.ST

type Input = (Vector (Vector Int))

readInput :: IO Input
readInput = readFile "input/15"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap (read . (:[])))
  <&> fmap V.fromList
  <&> V.fromList

type MGrid s = Vector (MVector s Int)
type M s a = ReaderT (Input, MGrid s) (ST s) a

search :: Int -> Int -> Int -> M s ()
search x y risk = do
  (grid, state) <- ask
  let h = V.length grid
  let w = V.length (V.head grid)
  best <- state V.! (h-1) `MV.read` (w-1)
  if | x < 0  -> pure ()
     | y < 0  -> pure ()
     | y >= h -> pure ()
     | x >= w -> pure ()
     | risk >= best -> pure ()
     | otherwise -> do
         let row = state V.! y
         cell <- row `MV.read` x
         let myrisk = risk + grid V.! y V.! x
         if | myrisk >= cell -> pure ()
            | otherwise -> do
                MV.write row x myrisk
                search (x + 1) y myrisk
                search x (y + 1) myrisk
                search (x - 1) y myrisk
                search x (y - 1) myrisk

solve :: Input -> Int
solve grid =
  let h = V.length grid
      w = V.length (V.head grid)
  in runST $ do
    state <- V.replicateM h $ MV.replicate w maxBound
    runReaderT (search 0 0 (0 - (grid V.! 0 V.! 0))) (grid, state)
    m <- traverse V.freeze state
    pure $ m V.! (h-1) V.! (w-1)

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
  & \grid' -> add grid' <$> [0..4]
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
