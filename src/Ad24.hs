{-# LANGUAGE ViewPatterns #-}

module Ad24 (main1, main2) where

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.Functor

data Op = Inp Int
        | Add Int Operand
        | Mul Int Operand
        | Div Int Operand
        | Mod Int Operand
        | Eql Int Operand
  deriving Show

data Chunk = Less Int Int | More Int
  deriving Show

data Operand = Reg Int | Lit Int
  deriving Show

regStrs :: [(String, Int)]
regStrs = zip ((:[]) <$> ['w'..'z']) [0..]

readReg :: String -> Maybe Int
readReg = flip lookup regStrs

readOperand :: String -> Operand
readOperand s = fromJust $ (Reg <$> readReg s) <|> Just (Lit $ read s)

readOperands :: String -> [Operand]
readOperands = fmap readOperand . words

parseOp :: String -> Op
parseOp ('i' : 'n' : 'p' : (readOperands -> [Reg a])) = Inp a
parseOp ('a' : 'd' : 'd' : (readOperands -> [Reg a, b])) = Add a b
parseOp ('m' : 'u' : 'l' : (readOperands -> [Reg a, b])) = Mul a b
parseOp ('d' : 'i' : 'v' : (readOperands -> [Reg a, b])) = Div a b
parseOp ('m' : 'o' : 'd' : (readOperands -> [Reg a, b])) = Mod a b
parseOp ('e' : 'q' : 'l' : (readOperands -> [Reg a, b])) = Eql a b
parseOp s = error $ "Bad op" <> s

isInp :: Op -> Bool
isInp (Inp _) = True
isInp _ = False

toChunks :: [Op] -> [Chunk]
toChunks [] = []
toChunks (inputOp:ops) =
  let (ops', rest) = first (inputOp:) $ break isInp ops
  in (:toChunks rest) $ case (ops' !! 4, ops' !! 5, ops' !! 15) of
    (Div _  (Lit 1), _, Add _ (Lit n)) -> More n
    (Div _ (Lit 26), Add _ (Lit n), Add _ (Lit m)) -> Less n m
    _ -> error "Couldn't parse chunk"

readInput :: IO [Chunk]
readInput = readFile "input/24"
  <&> lines
  <&> filter (/= "")
  <&> fmap parseOp
  <&> toChunks

evalChunk :: Int -> Chunk -> Int -> Int
evalChunk acc (More n) input = acc * 26 + input + n
evalChunk acc (Less c n) input =
  let (d, m) = acc `divMod` 26
  in if input == m + c then d else d * 26 + input + n

solve' :: [Int] -> Int -> [Int] -> [Chunk] -> Maybe [Int]
solve' _ 0 _ [] = Just []
solve' _ _ _ [] = Nothing
solve' _ _ [] _ = Nothing
solve' digs acc (input : rest) (chunk : chunks)
  = let res = evalChunk acc chunk input
    in case (chunk, res < acc) of
        -- Marie Kondo the heck outta this search space
        (Less _ _, False) -> rec acc rest (chunk : chunks)
        _ -> ((input:) <$> rec res digs chunks) <|> rec acc rest (chunk : chunks)
   where
     rec = solve' digs

solve :: [Int] -> [Chunk] -> Maybe [Int]
solve digs = solve' digs 0 digs

main :: [Int] -> IO ()
main digs = readInput >>= print . fmap (concatMap show) . solve digs

main1 :: IO ()
main1 = main $ reverse [1..9]

main2 :: IO ()
main2 = main [1..9]
