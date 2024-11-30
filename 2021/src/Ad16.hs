{-# LANGUAGE NamedFieldPuns #-}

module Ad16 (main1, main2) where

import Control.Arrow
import Data.Bits (shift)
import Data.List
import Numeric (readHex)
import Text.Printf (printf)

readInput :: IO String
readInput = readFile "input/16"

data Packet
  = Packet { version :: Int , typeId :: Int, packetData :: PacketData }
  deriving Show

data Op = Sum | Prod | Min | Max | Gt | Lt | Eq
  deriving Show

data PacketData
  = Operator Op [Packet]
  | Literal Int
  deriving Show

hexToBin :: Char -> String
hexToBin c
  = case readHex [c] of
      (x,_):_ -> printf "%04b" (x::Int)
      _       -> []

transform :: String -> [Bool]
transform = foldr ((<>) . fmap (== '1') . hexToBin) []

binToInt :: [Bool] -> Int
binToInt = foldl' step 0
  where
    step acc True = shift acc 1 + 1
    step acc _   = shift acc 1

parseLiteral :: Int -> [Bool] -> (PacketData, [Bool])
parseLiteral n (notEnd : a1 : a2 : a3 : a4 : xs)
  = let n' = n * 16 + binToInt [a1, a2, a3, a4]
    in if notEnd
      then parseLiteral n' xs
      else (Literal n', xs)
parseLiteral _ _ = error "Bad literal packet"

parseOperator :: [Bool] -> ([Packet], [Bool])
parseOperator (False : xs) =
  let l = binToInt $ take 15 xs
      xs' = drop 15 xs
      subs = parsePackets (take l xs')
  in (subs, drop l xs')
parseOperator (True : xs) =
  let nsubs = binToInt $ take 11 xs
      xs' = drop 11 xs
      (subs, xs'') = parseSubs nsubs xs'
  in (subs, xs'')
parseOperator _ = error "Can't parse operator"

parseSubs :: Int -> [Bool] -> ([Packet], [Bool])
parseSubs 0 xs = ([], xs)
parseSubs n xs = case parsePacket xs of
  Just (p, xs') -> let (ps, xs'') = parseSubs (n - 1) xs'
                   in (p : ps, xs'')
  Nothing -> error $ "can't parse sub-packets: " <> show n <> "\n" <> show xs

opType :: Int -> Op
opType n = case n of
  0 -> Sum
  1 -> Prod
  2 -> Min
  3 -> Max
  5 -> Gt
  6 -> Lt
  7 -> Eq
  _ -> error "Unknown operator"

parsePacketData :: Int -> [Bool] -> (PacketData, [Bool])
parsePacketData 4 = parseLiteral 0
parsePacketData tid = first (Operator (opType tid)) . parseOperator

parsePacket :: [Bool] -> Maybe (Packet, [Bool])
parsePacket (v1 : v2 : v3 : t1 : t2 : t3 : xs) =
  let tid = binToInt [t1, t2, t3]
      (pd, rest) = parsePacketData tid xs
  in Just
  ( Packet
    { version = binToInt [v1, v2, v3]
    , typeId = tid
    , packetData = pd
    }
  , rest
  )
parsePacket _ = Nothing

parsePackets :: [Bool] -> [Packet]
parsePackets xs = case parsePacket xs of
  Just (p, r) -> p : parsePackets r
  Nothing -> []

sumVersions :: [Packet] -> Int
sumVersions [] = 0
sumVersions (Packet{version, packetData} : xs)
  = sumVersions xs + version + case packetData of
      Operator _ subs -> sumVersions subs
      _ -> 0

eval :: Packet -> Int
eval Packet{packetData = Literal a} = a
eval Packet{packetData = Operator op subs'} =
  case (op, fmap eval subs') of
    (Sum, subs) -> sum subs
    (Prod, subs) -> product subs
    (Min, subs) -> minimum subs
    (Max, subs) -> maximum subs
    (Gt, [a, b]) -> if a > b then 1 else 0
    (Lt, [a, b]) -> if a < b then 1 else 0
    (Eq, [a, b]) -> if a == b then 1 else 0
    _ -> error "Bad operator application"

solve1 :: String -> Int
solve1 = transform
  >>> parsePackets
  >>> sumVersions

solve2 :: String -> Int
solve2 = transform
  >>> parsePackets
  >>> head
  >>> eval

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
