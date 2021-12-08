module Ad08 where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import Data.Functor
import Debug.Trace

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

fl :: Int -> [String] -> [String]
fl n = filter ((== n) . length)

diff :: [String] -> Int -> Int -> String
diff xs n m = head (fl n xs) \\ head (fl m xs)

missing :: String -> String
missing s = ['a'..'g'] \\ s

fc :: Char -> [String] -> [String]
fc c = filter (c `elem`)

fmis :: Char -> [String] -> [String]
fmis c = filter (not . (c `elem`))

findMapping :: [String] -> [String]
findMapping xs =
  let a = head $ diff xs 3 2
      one = head $ fl 2 xs
      four = head $ fl 4 xs
      eight = head $ fl 7 xs
      seven = head $ fl 3 xs
      fives = fl 5 xs
      sixes = fl 6 xs
      ninezero = sixes & fc (one !! 1) & fc (head one)
      six = head $ sixes \\ ninezero
      c = head $ missing six
      f = head $ one \\ [c]
      two = fives & fmis f & head
      b = missing two \\ [f] & head
      three = fives & fmis b & fmis e & head
      five = fl 5 xs & fmis c & head
      e = head $ missing five \\ [c]
      nine = sixes & fmis e & head
      zero = sixes & fc c & fc e & head
  in sort <$> [zero, one, two, three, four, five, six, seven, eight, nine]

solveLine1 :: Line -> Int
solveLine1 (ins, outs) =
  let l = findMapping ins
      wanted = (l !! 1) : (l !! 4) : (l !! 7) : (l !! 8) : []
  in
  length $ filter (`elem` wanted) outs

fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' (x:xs) = x + 10 * fromDigits' xs

fromDigits :: [Int] -> Int
fromDigits = fromDigits' . reverse

solveLine2 :: Line -> Int
solveLine2 (ins, outs) =
  let l = findMapping ins
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
