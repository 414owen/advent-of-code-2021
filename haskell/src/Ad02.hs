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

follow1 :: (Int, Int) -> Instruction -> (Int, Int)
follow1 (depth, x) = \case
  Up n -> (depth - n, x)
  Down n -> (depth + n, x)
  Forward n -> (depth, x + n)

solve1 :: [Instruction] -> Int
solve1 = uncurry (*) . foldl' follow1 (0, 0)

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

follow2 :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
follow2 (aim, depth, x) = \case
  Up n -> (aim - n, depth, x)
  Down n -> (aim + n, depth, x)
  Forward n -> (aim, depth + aim * n, x + n)

solve2 :: [Instruction] -> Int
solve2 insts = foldl' follow2 (0, 0, 0) insts & \case
  (_, d, p) -> d * p

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
