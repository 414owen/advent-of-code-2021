{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Ad22 (main1, main2) where

import Control.Category ((>>>))
import Control.Monad
import Data.Char
import Data.Functor
import Data.Function
import Data.Foldable

type Rng = (Int, Int)
data Cube = Cube Rng Rng Rng
  deriving Show
data Switch = On Cube | Off Cube
  deriving Show

readCube :: String -> Cube
readCube = fmap (\a -> if isDigit a || a == '-' then a else ' ' )
  >>> words
  >>> fmap read
  >>> \case
    [ x1, x2, y1, y2, z1, z2 ] -> Cube (x1, x2) (y1, y2) (z1, z2)
    _ -> error "bad cube"

readSwitch :: String -> Switch
readSwitch ('o' : 'n' : xs) = On $ readCube xs
readSwitch xs = Off $ readCube xs

readInput :: IO [Switch]
readInput = readFile "input/22"
  <&> lines
  <&> filter (/= "")
  <&> fmap readSwitch

splitRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
splitRange r1@(x1, x2) r2@(x3, x4)
  -- 1 inside 2
  | x1 > x3 && x2 < x4 = [(x3, x1 - 1), (x1, x2), (x2 + 1, x4)]
  -- 2 inside 1
  | x1 < x3 && x2 > x4 = [(x1, x3 - 1), (x3, x4), (x4 + 1, x2)]
  -- 1 1&2 2
  | x1 < x3 && x2 < x4 = [(x1, x3 - 1), (x3, x2), (x2 + 1, x4)]
  -- 2 1&2 1
  | x1 > x3 && x2 > x4 = [(x3, x1 - 1), (x1, x4), (x4 + 1, x2)]

  | x1 == x3 && x2 == x4 = [(x1, x2)]
  | x1 == x3 && x2 < x4 = [(x1, x2), (x2 + 1, x4)]
  | x1 == x3 && x2 > x4 = [(x1, x4), (x4 + 1, x2)]
  | x2 == x4 && x1 > x3 = [(x3, x1 - 1), (x1, x2)]
  | x2 == x4 && x1 < x3 = [(x1, x3 - 1), (x3, x2)]

  | otherwise = error $ "impossible: " <> show [r1, r2]

hasOverlap :: (Int, Int) -> (Int, Int) -> Bool
hasOverlap (x1, x2) (x3, x4)
  | x1 >= x3 && x1 <= x4 = True
  | x2 >= x3 && x2 <= x4 = True
  | x3 >= x1 && x3 <= x2 = True
  | x4 >= x1 && x4 <= x2 = True
  | otherwise = False

cubesOverlap :: Cube -> Cube -> Bool
cubesOverlap (Cube xr1 yr1 zr1) (Cube xr2 yr2 zr2) =
  [ (xr1, xr2), (yr1, yr2), (zr1, zr2) ]
  <&> uncurry hasOverlap & and

diff :: [Cube] -> Cube -> [Cube]
diff [] _ = []
diff (c1@(Cube xr1 yr1 zr1) : cs) c2@(Cube xr2 yr2 zr2)
  | cubesOverlap c1 c2 = (<> diff cs c2) $ do
        xr3 <- splitRange xr1 xr2
        yr3 <- splitRange yr1 yr2
        zr3 <- splitRange zr1 zr2
        let c3 = Cube xr3 yr3 zr3
        guard $ cubesOverlap c1 c3
        guard $ not $ cubesOverlap c2 c3
        pure $ c3
  | otherwise = c1 : diff cs c2

switch :: [Cube] -> Switch -> [Cube]
switch cs (On c) = (foldl' diff [c] cs) <> cs
switch cs (Off c) = diff cs c

onCubes :: [Switch] -> [Cube]
onCubes = foldl' switch []

count :: Cube -> Int
count (Cube (x1, x2) (y1, y2) (z1, z2)) =
  (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) * (abs (z2 - z1) + 1)

solve :: [Switch] -> Int
solve = onCubes
  >>> fmap count
  >>> sum

limit50' :: Cube -> Bool
limit50' (Cube xr yr zr) = (l xr) && (l yr) && (l zr)
  where
    l (a, b) = l' a && l' b
    l' a = a >= negate 50 && a <= 50


limit50 :: Switch -> Bool
limit50 (On c) = limit50' c
limit50 (Off c) = limit50' c

main1 :: IO ()
main1 = readInput >>= print . solve . filter limit50

main2 :: IO ()
main2 = readInput >>= print . solve

