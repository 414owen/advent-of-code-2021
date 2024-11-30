{-# LANGUAGE MultiWayIf #-}

module Ad15 (main1, main2) where

import Control.Category ((>>>))
import Data.Set (Set)
import qualified Data.Set as S
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

type Heap = Set (Int, Int, Int)

search :: Heap -> M s Int
search heap = do
  let Just ((risk, x, y), heap') = S.minView heap
  (grid, state) <- ask
  let h = V.length grid
  let w = V.length (V.head grid)
  if | x < 0  -> search heap'
     | y < 0  -> search heap'
     | y >= h -> search heap'
     | x >= w -> search heap'
     | otherwise -> do
         let row = state V.! y
         let myrisk = risk + grid V.! y V.! x
         cell <- row `MV.read` x
         if | myrisk >= cell -> search heap'
            | (w-1, h-1) == (x, y) -> pure myrisk
            | otherwise -> do
                MV.write row x myrisk
                search $ S.insert (myrisk, x, y + 1)
                       $ S.insert (myrisk, x, y - 1)
                       $ S.insert (myrisk, x + 1, y)
                       $ S.insert (myrisk, x - 1, y)
                         heap'

solve :: VInput -> Int
solve grid =
  let h = V.length grid
      w = V.length (V.head grid)
  in runST $ do
    state <- V.replicateM h $ MV.replicate w maxBound
    flip runReaderT (grid, state)
      $ search
      $ S.singleton (negate $ grid V.! 0 V.! 0, 0, 0)

main1 :: IO ()
main1 = readInput >>=
  (   fmap V.fromList
  >>> V.fromList
  >>> solve
  >>> print
  )

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
main2 = readInput >>=
  (   construct
  >>> fmap V.fromList
  >>> V.fromList
  >>> solve
  >>> print
  )
