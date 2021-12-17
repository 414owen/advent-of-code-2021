{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

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
import Debug.Trace

type Input = [[Int]]
type VInput = Vector (Vector Int)
type Pt = (Int, Int)

readInput :: IO Input
readInput = readFile "input/15"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap (read . (:[])))

type MGrid s = Vector (MVector s Int)
type M s a = ReaderT (VInput, MGrid s) (ST s) a

newtype Elem = Elem (Int, Int, Int)
  deriving Eq
type Heap = Set Elem

instance Ord Elem where
  compare (Elem e1@(a, b, c)) (Elem e2@(d, e, f))
    | a == d = compare ((e + f), e, f) ((b + c), b, c)
    | otherwise = compare e1 e2

search :: Heap -> M s Int
search heap = do
  let Elem ((risk, x, y)) = S.findMin heap
  let heap' = S.deleteMin heap
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
                let nh = S.insert (Elem (myrisk, x, y + 1))
                       $ S.insert (Elem (myrisk, x, y - 1))
                       $ S.insert (Elem (myrisk, x + 1, y))
                       $ S.insert (Elem (myrisk, x - 1, y))
                       $ heap'
                search nh

solve :: VInput -> Int
solve grid =
  let h = V.length grid
      w = V.length (V.head grid)
  in runST $ do
    state <- V.replicateM h $ MV.replicate w maxBound
    flip runReaderT (grid, state)
      $ (search $ S.singleton $ Elem (negate $ grid V.! 0 V.! 0, 0, 0))

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
