module Ad10 where

import Control.Category ((>>>))
import Data.List
import Data.Maybe
import Data.Function
import Data.Functor

readInput :: IO [String]
readInput = readFile "input/10"
  <&> lines
  <&> (\\ [""])

isClosing :: Char -> Bool
isClosing = (`elem` ">]})")

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match _ _ = False

corrupted :: String -> String -> Either Char String
corrupted s [] = Right s
corrupted [] (x:xs)
  | isClosing x = Left x
  | otherwise = corrupted [x] xs
corrupted (y:ys) (x:xs)
  | isClosing x && match y x = corrupted ys xs
  | isClosing x = Left x
  | otherwise = corrupted (x:y:ys) xs

corrupteds :: [String] -> [Either Char String]
corrupteds = fmap (corrupted "")

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = error "Unknown bracket!"

solve1 :: [String] -> Int
solve1 = corrupteds
  >>> fmap (either Just (const Nothing))
  >>> catMaybes
  >>> fmap score
  >>> sum

score2 :: Char -> Int
score2 '(' = 1
score2 '[' = 2
score2 '{' = 3
score2 '<' = 4
score2 _ = error "Unknown bracket!"

score2' :: String -> Int
score2' [] = 0
score2' (x:xs) = score2 x + 5 * score2' xs

solve2 :: [String] -> Int
solve2 xs =
  let l = corrupteds xs
          & fmap (either (const Nothing) Just)
          & catMaybes
          & fmap reverse
          & fmap score2'
          & sort
  in l !! (length l `div` 2)

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
