{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}

module Ad24 (main1, main2) where

import Control.Arrow
import Control.Applicative
import Control.Monad.ST
import Data.Char
import Data.Maybe
import Data.List
import Data.Functor
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

data Op = Inp Int
        | Add Int Operand
        | Mul Int Operand
        | Div Int Operand
        | Mod Int Operand
        | Eql Int Operand
  deriving Show

data Chunk = Less [Op] | More [Op]
  deriving Show

data Operand = Reg Int | Lit Int

instance Show Operand where
  show (Reg x) = "Reg " <> [['w'..] !! x]
  show (Lit n) = "Lit " <> show n

regStrs :: [(String, Int)]
regStrs = zip ((:[]) <$> ['w'..'z']) [0..]

nRegs :: Int
nRegs = length regStrs

zReg :: Int
zReg = nRegs - 1

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
      chunks = toChunks rest
  in (:chunks) $ case ops' !! 4 of
    Div _ (Lit 1) -> More ops'
    _ -> Less ops'

readInput :: IO [Chunk]
readInput = readFile "input/24"
  <&> lines
  <&> takeWhile (/= "END")
  <&> filter (/= "")
  <&> filter (not . ("--" `isPrefixOf`))
  <&> fmap parseOp
  <&> toChunks

type Regs s = MVector s Int

getNum :: Operand -> Regs s -> ST s Int
getNum (Lit n) _ = pure n
getNum (Reg reg) regs = MV.read regs reg

interpret :: [Int] -> [Op] -> Regs s -> ST s ()
interpret _ [] _ = pure ()
interpret [] (Inp _ : _) _ = error "not enough input!"
interpret (x : xs) (Inp reg : ops) regs = do
  MV.write regs reg $ fromIntegral x
  interpret xs ops regs
interpret xs (Add reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs + rhs
  interpret xs ops regs
interpret xs (Mul reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs * rhs
  interpret xs ops regs
interpret xs (Div reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `div` rhs
  interpret xs ops regs
interpret xs (Mod reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `mod` rhs
  interpret xs ops regs
interpret xs (Eql reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ if lhs == rhs then 1 else 0
  interpret xs ops regs

intToStr :: Int -> String
intToStr 0 = []
intToStr n = chr (ord 'a' + fromIntegral (n `mod` 26))
           : intToStr (n `div` 26)

chunkOps :: Chunk -> [Op]
chunkOps (More ops) = ops
chunkOps (Less ops) = ops

evalChunk :: Int -> Chunk -> Int -> Int
evalChunk acc chunk input = runST $ do
  regs <- MV.replicate nRegs 0
  MV.write regs zReg acc
  interpret [input] (chunkOps chunk) regs
  MV.read regs zReg

solve' :: [Int] -> Int -> [Int] -> [Chunk] -> Maybe [Int]
solve' _ 0 _ [] = Just []
solve' _ _ _ [] = Nothing
solve' _ _ [] _ = Nothing
solve' digs acc (input : rest) (chunk : chunks)
  = let res = evalChunk acc chunk input
    in case (chunk, res < acc) of
        (Less _, False) -> rec acc rest (chunk : chunks)
        _ -> ((input:) <$> rec res digs chunks) <|> rec acc rest (chunk : chunks)
   where
     rec = solve' digs

solve :: [Int] -> [Chunk] -> Maybe [Int]
solve digs chunks = solve' digs 0 digs chunks

main :: [Int] -> IO ()
main digs = readInput >>= print . fmap (concatMap show) . solve digs

main1 :: IO ()
main1 = main $ reverse [1..9]

main2 :: IO ()
main2 = main [1..9]
