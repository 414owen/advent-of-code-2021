{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}

module Ad24 (main1) where

import Data.Char
import Data.Maybe
import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Debug.Trace

data Op = Inp Int
        | Add Int Operand
        | Mul Int Operand
        | Div Int Operand
        | Mod Int Operand
        | Eql Int Operand
  deriving Show

data Chunk = Less [Op] | More [Op]
  deriving Show

data Operand = Reg Int | Lit Integer
  deriving Show

regStrs :: [(String, Int)]
regStrs = (zip ((:[]) <$> ['w'..'z']) [0..])

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
parseOp _ = error "Bad op"

readInput :: IO [Op]
readInput = readFile "input/24"
  <&> lines
  <&> takeWhile (/= "END")
  <&> filter (/= "")
  <&> filter (not . ("--" `isPrefixOf`))
  <&> fmap parseOp

type Regs s = MVector s Integer

getNum :: Operand -> Regs s -> ST s Integer
getNum (Lit n) _ = pure n
getNum (Reg reg) regs = MV.read regs reg

interpret' :: [Int] -> [Op] -> Regs s -> ST s ()
interpret' _ [] regs = pure ()
interpret' [] (Inp reg : ops) regs = error "not enough input!"
interpret' (x : xs) (Inp reg : ops) regs = do
  MV.write regs reg $ fromIntegral x
  interpret xs ops regs
interpret' xs (Add reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs + rhs
  interpret xs ops regs
interpret' xs (Mul reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs * rhs
  interpret xs ops regs
interpret' xs (Div reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `div` rhs
  interpret xs ops regs
interpret' xs (Mod reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ lhs `mod` rhs
  interpret xs ops regs
interpret' xs (Eql reg op : ops) regs = do
  rhs <- getNum op regs
  lhs <- MV.read regs reg
  MV.write regs reg $ if lhs == rhs then 1 else 0
  interpret xs ops regs

interpret :: [Int] -> [Op] -> Regs s -> ST s ()
interpret a [] regs = pure ()
interpret a (op:b) regs = do
  regs' <- (\r -> traceShow (op, r) r) <$> V.freeze regs
  regs'' <- V.thaw regs'
  interpret' a b regs''

-- runChunk :: Int -> Chunk -> Maybe [Int]
-- runChunk acc (More ops) = runST
--   regs <- MV.replicate nRegs 0
--   MV.write regs zreg acc
--   acc' interpret

digs :: [Int]
digs = reverse [1..9]

intToStr :: Integer -> String
intToStr 0 = []
intToStr n = chr (ord 'a' + fromIntegral (n `mod` 26))
           : intToStr (n `div` 26)

solve1 :: [Op] -> [String]
solve1 ops = do
  x1  <- digs
  x2  <- digs
  x3  <- digs
  x4  <- digs
  -- x5  <- digs
  -- x6  <- digs
  -- x7  <- digs
  -- x8  <- digs
  -- x9  <- digs
  -- x10 <- digs
  -- x11 <- digs
  -- x12 <- digs
  -- x13 <- digs
  -- x14 <- digs
  let input =
        [ x1
        , x2
        , x3
        , x4
        -- , x5
        -- , x6
        -- , x7
        -- , x8
        -- , x9
        -- , x10
        -- , x11
        -- , x12
        -- , x13
        -- , x14
        ]
  let res = runST $ do
        regs <- MV.replicate nRegs 0
        interpret input ops regs
        MV.read regs zReg
  [show $ (res, reverse $ intToStr res, input)]
    <> if x4 == 1 then [replicate 40 '-'] else []

main1 :: IO ()
main1 = readInput >>= mapM_ putStrLn . solve1
