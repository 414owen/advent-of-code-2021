{-# LANGUAGE LambdaCase #-}

module Ad04 where

import Control.Category ((>>>))
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor

type Board = [[Maybe Int]]

readBoards :: [[Maybe Int]] -> [Board]
readBoards [] = []
readBoards xs = take 5 xs : readBoards (drop 5 xs)

readInput :: IO ([Int], [Board])
readInput = readFile "input/04"
  <&> lines
  <&> filter (/= "")
  <&> \case
    (nums : boards) ->
      ( read ("[" <> nums <> "]")
      , readBoards $ (fmap (Just . read) . words) <$> boards
      )

markCell :: Int -> Maybe Int -> Maybe Int
markCell x Nothing = Nothing
markCell x (Just a)
  | x == a = Nothing
  | otherwise = Just a

mark :: Int -> Board -> Board
mark n bs = fmap (markCell n) <$> bs

markAll :: Int -> [Board] -> [Board]
markAll x bs = fmap (mark x) bs

hasWon' :: Board -> Bool
hasWon' = any (all (== Nothing))

hasWon :: Board -> Bool
hasWon b = hasWon' b || hasWon' (transpose b)

winner :: [Board] -> Maybe Board
winner [] = Nothing
winner (b:bs)
  | hasWon b = Just b
  | otherwise = winner bs

score :: Board -> Int
score = mconcat
  >>> catMaybes
  >>> sum

solve1 :: [Int] -> [Board] -> Int
solve1 [] bs = error "no winner"
solve1 (x:xs) bs =
  let bs' = markAll x bs
  in case winner bs' of
    Just b -> score b * x
    Nothing -> solve1 xs bs'

solve2 :: [Int] -> [Board] -> Int
solve2 (x:xs) [b] =
  let b' = mark x b
  in if hasWon b'
    then score b' * x
    else solve2 xs [b']
solve2 (x:xs) bs =
  let bs' = markAll x bs
  in solve2 xs (filter (not . hasWon) bs')
solve2 _ _ = error "no last winner"

main1 :: IO ()
main1 = readInput
  <&> uncurry solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> uncurry solve2
  >>= print
