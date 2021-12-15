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

type Input = [[Int]]
type VInput = Vector (Vector Int)

readInput :: IO Input
readInput = readFile "input/15"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap (read . (:[])))

type MGrid s = Vector (MVector s Int)
type M s a = ReaderT (VInput, MGrid s) (ST s) a

search :: Int -> Int -> Int -> M s ()
search x y risk = do
  (grid, state) <- ask
  let h = V.length grid
  let w = V.length (V.head grid)
  if | x < 0  -> pure ()
     | y < 0  -> pure ()
     | y >= h -> pure ()
     | x >= w -> pure ()
     | otherwise -> do
         let row = state V.! y
         let myrisk = risk + grid V.! y V.! x
         best <- state V.! (h-1) `MV.read` (w-1)
         cell <- row `MV.read` x
         if | myrisk >= cell -> pure ()
            | myrisk >= best -> pure ()
            | otherwise -> do
                MV.write row x myrisk
                search (x + 1) y myrisk
                search x (y + 1) myrisk
                search (x - 1) y myrisk
                search x (y - 1) myrisk

solve :: VInput -> Int
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
  <&> fmap V.fromList
  <&> V.fromList
  <&> solve
  >>= print

add :: (Functor f, Functor g, Integral a) => f (g a) -> a -> f (g a)
add grid n = fmap ((+ 1) . (`mod` 9) . (\a -> a - 1) . (+ n)) <$> grid

construct :: Input -> Input
construct grid = add grid <$> [0..4]
  & concat
  & transpose
  & (\g -> add g <$> [0..4])
  & concat
  & transpose

main2 :: IO ()
main2 = readInput
  <&> construct
  <&> fmap V.fromList
  <&> V.fromList
  <&> solve
  >>= print
