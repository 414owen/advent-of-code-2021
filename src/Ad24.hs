{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}

module Ad24 (main1, main2) where

import Data.Char
import Data.Maybe
import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad.ST
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

data Operand = Reg Int | Lit Int

instance Show Operand where
  show (Reg x) = "Reg " <> ((['w'..] !! x):[])
  show (Lit n) = "Lit " <> show n

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
parseOp s = error $ "Bad op" <> s

isInp :: Op -> Bool
isInp (Inp _) = True
isInp _ = False

toChunks :: [Op] -> [Chunk]
toChunks [] = []
toChunks (_:ops) =
  let (c, rest) = break isInp ops
      chunks = toChunks rest
  in (:chunks) $ case c of
    ops' -> case ops' !! 3 of
      Div _ (Lit 1) -> More $ ops'
      _ -> Less $ ops'

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

-- runChunk :: Int -> Chunk -> Maybe [Int]
-- runChunk acc (More ops) = runST
--   regs <- MV.replicate nRegs 0
--   MV.write regs zreg acc
--   acc' interpret

intToStr :: Int -> String
intToStr 0 = []
intToStr n = chr (ord 'a' + fromIntegral (n `mod` 26))
           : intToStr (n `div` 26)

solve :: [Int] -> Int -> [Int] -> [Chunk] -> Maybe [Int]
solve _ 0 _ [] = Just []
solve _ _ _ [] = Nothing
solve _ _ [] _ = Nothing
solve digs acc (input : rest) (chunk : chunks)
  = runST $ do
      regs <- MV.replicate nRegs 0
      MV.write regs zReg acc
      MV.write regs 0 input
      let ops = case chunk of
            More a -> a
            Less a -> a
      interpret [] ops regs
      res <- MV.read regs zReg
      case (chunk, res < acc) of
        (Less _, False) -> pure $ solve digs acc rest (chunk : chunks)
        _ -> pure $ (input:) <$> solve digs res digs chunks
               <|> solve digs acc rest (chunk : chunks)

main1 :: IO ()
main1 = readInput >>= print . fmap (concatMap show) . solve (reverse [1..9]) 0 (reverse [1..9])

main2 :: IO ()
main2 = readInput >>= print . fmap (concatMap show) . solve [1..9] 0 [1..9]
