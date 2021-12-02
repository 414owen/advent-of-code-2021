{-# LANGUAGE LambdaCase #-}

module Ad02 where

import Data.Function
import Data.Functor
import Data.Foldable

data Instruction
  = Up Int
  | Down Int
  | Forward Int

parseInst :: [String] -> Instruction
parseInst = \case
  ["forward", n] -> Forward (read n)
  ["down", n] -> Down (read n)
  ["up", n] -> Up (read n)
  _ -> error "Bad input"

readInput :: IO [Instruction]
readInput = readFile "input/02"
  <&> lines
  <&> filter (/= "")
  <&> fmap words
  <&> fmap parseInst

follow :: (Int, Int) -> Instruction -> (Int, Int)
follow (depth, x) = \case
  Up n -> (depth - n, x)
  Down n -> (depth + n, x)
  Forward n -> (depth, x + n)

solve1 :: [Instruction] -> Int
solve1 = uncurry (*) . foldl' follow (0, 0)

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print
