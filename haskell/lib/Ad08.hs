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

eight :: String
eight = ['a'..'g']

-- filter by length
fl :: Int -> [String] -> [String]
fl n = filter ((== n) . length)

-- filter by element char
fc :: Char -> [String] -> [String]
fc c = filter (c `elem`)

-- filter by string contains
fs :: String -> [String] -> [String]
fs = flip $ foldl' $ flip fc

-- filter by missing char
fmis :: Char -> [String] -> [String]
fmis c = filter (not . (c `elem`))

findDigits :: [String] -> [String]
findDigits xs =
  let one = head $ fl 2 xs
      four = head $ fl 4 xs
      seven = head $ fl 3 xs
      a = head $ seven \\ one
      sixes = fl 6 xs
      six = head $ sixes \\ fs one sixes
      c = head $ eight \\ six
      f = head $ one \\ [c]
      fives = fl 5 xs
      two = fives & fmis f & head
      b = (eight \\ two) \\ [f] & head
      three = fives & fmis b & fmis e & head
      five = fl 5 xs & fmis c & head
      e = head $ (eight \\ five) \\ [c]
      nine = sixes & fmis e & head
      zero = sixes \\ [nine, six] & head
  in sort <$> [zero, one, two, three, four, five, six, seven, eight, nine]

solveLine1 :: Line -> Int
solveLine1 (ins, outs) =
  let l = findDigits ins
      wanted = (l !! 1) : (l !! 4) : (l !! 7) : (l !! 8) : []
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
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
