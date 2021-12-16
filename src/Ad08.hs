module Ad08 where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import Data.Functor

type Line = ([String], [String])
type Input  = [Line]
type Output = Int

readInput :: IO Input
readInput = readFile "input/08"
  <&> lines
  <&> filter (/= "")
  <&> fmap words
  <&> fmap (liftM2 (,)
    (takeWhile (/= "|"))
    (fmap sort . tail . dropWhile (/= "|")))

-- filter by length
fl :: Int -> [String] -> [String]
fl n = filter ((== n) . length)

-- filter by element char
fc :: Char -> [String] -> [String]
fc c = filter (c `elem`)

-- filter by string contains
fs :: String -> [String] -> [String]
fs = flip $ foldl' $ flip fc

findDigits :: [String] -> [String]
findDigits xs =
  let sixes = fl 6 xs
      fives = fl 5 xs

      zero = sixes \\ [nine, six] & head
      one = head $ fl 2 xs
      two = fives \\ [five, three] & head
      three = fives & fs one & head
      four = head $ fl 4 xs
      five = fives & find ((== six) . union six) & fromJust
      six = head $ sixes \\ fs one sixes
      seven = head $ fl 3 xs
      eight = ['a'..'g']
      nine = sixes & fs four & head

  in sort <$> [zero, one, two, three, four, five, six, seven, eight, nine]

solveLine1 :: Line -> Int
solveLine1 (ins, outs) =
  let l = findDigits ins
      wanted = [l !! 1, l !! 4, l !! 7, l !! 8]
  in length $ filter (`elem` wanted) outs

fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' (x:xs) = x + 10 * fromDigits' xs

fromDigits :: [Int] -> Int
fromDigits = fromDigits' . reverse

solveLine2 :: Line -> Int
solveLine2 (ins, outs) =
  let l = findDigits ins
      digs = fmap (fromJust . flip elemIndex l) outs
  in fromDigits digs

solve1 :: Input -> Output
solve1 = sum . fmap solveLine1

solve2 :: Input -> Output
solve2 = sum . fmap solveLine2

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
