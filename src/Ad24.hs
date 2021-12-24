{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ad24 where

import Data.Maybe
import Data.Functor
import Control.Applicative
import Control.Monad.ST
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

data Op = Inp Int
        | Add Int Operand
        | Mul Int Operand
        | Div Int Operand
        | Mod Int Operand
        | Eql Int Operand
  deriving Show

data Operand = Reg Int | Lit Int
  deriving Show

regStrs :: [(String, Int)]
regStrs = zip ((:[]) <$> ['w'..'z']) [0..]

zVar :: Int
zVar = fromJust $ lookup "z" regStrs

nRegs :: Int
nRegs = length regStrs

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
parseOp _ = error "Bad op"

readInput :: IO [Op]
readInput = readFile "input/24"
  <&> lines
  <&> filter (/= "")
  <&> filter ("--" `isPrefixOf`)
  <&> fmap parseOp

getNum :: Operand -> MVector s Int -> ST s Int
getNum (Lit n) _ = pure n
getNum (Reg reg) regs = MV.read regs reg

interpret :: forall s. [Op] -> MVector s Int -> ST s (Maybe [Int])
interpret [] regs = do
  n <- MV.read regs zVar
  pure $ case n of
    0 -> Just []
    _ -> Nothing
interpret (Inp reg : ops) regs = do
  res <- MV.read regs zVar
  case res of
    0 -> rec digs
    _ -> pure $ Nothing

  where
    rec :: [Int] -> ST s (Maybe [Int])
    rec [] = pure Nothing
    rec (x : xs) = do
      clone <- MV.clone regs
      MV.write clone reg x
      res <- interpret ops clone
      case res of
        Just ys -> pure $ Just $ x : ys
        Nothing -> rec xs
interpret (Add reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs + rhs
  interpret ops regs
interpret (Mul reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs * rhs
  interpret ops regs
interpret (Div reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `div` rhs
  interpret ops regs
interpret (Mod reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `mod` rhs
  interpret ops regs
interpret (Eql reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ if lhs == rhs then 1 else 0
  interpret ops regs

digs :: [Int]
digs = reverse [1..9]

solve1 :: [Op] -> Maybe [Int]
solve1 ops = runST $ do
  regs <- MV.replicate nRegs 0
  interpret ops regs

constraints :: [(Int, Int) -> Bool]
constraints =
  [ \(z, w) -> z `mod` 26 + 15 == w
  , \(


  , \(
  ]

main1 :: IO ()
main1 = readInput >>= print . solve1
