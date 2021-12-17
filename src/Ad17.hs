{-# LANGUAGE LambdaCase #-}

module Ad17 (main1, main2) where

import Control.Category ((>>>))
import Data.Char
import Data.Function
import Data.Functor

type Input = (Int, Int, Int, Int)

readInput :: IO Input
readInput = readFile "input/17"
  <&> fmap (\c -> if isDigit c || c == '-' then c else ' ')
  <&> words
  <&> fmap read
  <&> \case
        [x1, x2, y1, y2] -> (x1, x2, y1, y2)
        _ -> error "Bad input"

valid :: Input -> (Int, Int) -> (Int, Int) -> Bool
valid inp@(x1, x2, y1, y2) (x, y) (vx, vy)
  | y < y1 = False
  | x > x2 = False
  | x >= x1 && y <= y2 = True
  | otherwise = valid inp (x + vx, y + vy) (vx - signum vx, vy - 1)

highestY :: (Int, Int) -> Int
highestY (_, vy) = sum [1..vy]

getValids :: Input -> [(Int, Int)]
getValids input@(_, x2, _, _) = [(x, y) | x <- [min 0 x2..max 0 x2], y <- [-500..500]]
  & filter (valid input (0, 0))

solve1 :: Input -> Int
solve1 = getValids
  >>> fmap highestY
  >>> maximum

solve2 :: Input -> Int
solve2 = getValids >>> length

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
