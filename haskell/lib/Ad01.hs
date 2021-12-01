module Ad01 where

import Data.Functor
import Control.Category hiding ((.))
import Data.Function
import Data.Foldable

solve1 :: [Int] -> Int
solve1 xs = zip xs (tail xs)
  & gts

gts :: [(Int, Int)] -> Int
gts = fmap (\(a, v) -> if v > a then 1 else 0)
  >>> sum

slids' :: Int -> [a] -> [[a]]
slids' n [] = []
slids' n xs = take n xs : slids n (tail xs)

slids :: Int -> [a] -> [[a]]
slids n xs = slids' n xs
  & filter ((== n) . length)

readInput :: IO [Int]
readInput = do
  file <- readFile "input/01"
  pure $ file
    & lines
    & filter (/= "")
    & fmap read

solve2 :: [Int] -> Int
solve2 xs = let ys = sum <$> slids 3 xs in
  zip ys (tail ys)
  & gts

main1 :: IO ()
main1 = solve1 <$> readInput
  >>= print

main2 :: IO ()
main2 = solve2 <$> readInput
  >>= print
