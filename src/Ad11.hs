{-# LANGUAGE MultiWayIf #-}

module Ad11 where

import Control.Monad.ST
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Functor
import Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Grid = Vector (Vector Int)
type MGrid s = MVector s (MVector s Int)

readInput :: IO Grid
readInput = readFile "input/11"
  <&> lines
  <&> filter (/= "")
  <&> fmap (fmap $ read . (:[]))
  <&> V.fromList
  <&> fmap V.fromList

type M s a = WriterT (DList (Int, Int)) (ReaderT (MGrid s) (ST s)) a

forM2 :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [[c]]
forM2 l1 l2 f = forM l1 $ \a -> forM l2 $ \b -> f a b

inc :: Int -> Int -> M s ()
inc x y = do
  (x, y) <- pure (x, y)
  grid <- ask
  if | x < 0   -> pure ()
     | y < 0   -> pure ()
     | x >= 10 -> pure ()
     | y >= 10 -> pure ()
     | otherwise -> do
         row <- MV.read grid y
         cell <- MV.read row x
         MV.write row x $ cell + 1
         case cell of
           9 -> do
             tell $ DL.singleton (x, y)
             void $ forM2 [x-1..x+1] [y-1..y+1] $
               \x' y' -> if (x, y) == (x', y')
                 then pure ()
                 else inc x' y'
           _ -> pure ()
         pure ()

step :: ReaderT (MGrid s) (ST s) Int
step = do
  grid <- ask
  toZero <- fmap DL.toList $ execWriterT $
    forM2 [0..9] [0..9] $ \x y -> inc x y
  forM_ toZero $ \(x, y) -> do
     row <- MV.read grid y
     MV.write row x 0
  pure $ length toZero

solve1 :: Grid -> Int
solve1 grid = runST $ do
  g <- V.thaw =<< traverse V.thaw grid
  fmap sum $ flip runReaderT g
    $ forM [(1::Int)..100]
    $ const step

solve2 :: Grid -> Int
solve2 grid = runST $ do
  g <- V.thaw =<< traverse V.thaw grid
  flip runReaderT g
    $ rec 1
  where
    rec :: Int -> ReaderT (MGrid s) (ST s) Int
    rec n = do
      m <- step
      if m == 100
        then pure n
        else rec $ n + 1

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
