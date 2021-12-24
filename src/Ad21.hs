{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ad21 (main1, main2) where

import Data.Char
import GHC.Generics (Generic)
import Data.Hashable
import Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import Data.Functor
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.ST
import Debug.Trace

readInput :: IO (Int, Int)
readInput = readFile "input/21"
  <&> lines
  <&> fmap reverse
  <&> fmap (takeWhile isDigit)
  <&> fmap reverse
  <&> filter (/= "")
  <&> fmap read
  <&> \case
    [a, b] -> (a - 1, b - 1)
    _ -> error "bad input"

data Turn = P1 | P2
  deriving (Eq, Generic, Show)
instance Hashable Turn
type Pos = Int
type Die = Int
type Score = Int
type Player = (Pos, Score)
data AState
  = AState
  { turn :: Turn
  , player1 :: Player
  , player2 :: Player
  , die :: Die
  }
  deriving Show
type M1 = State AState



-- ------
-- Part 1
-- ------

roll :: M1 Int
roll = do
  modify $ \case
    s@AState{ die } -> s{ die = die + 1 }
  AState{ die } <- get
  pure die

takeTurn1 :: Player -> M1 Player
takeTurn1 (pos, sc) = do
  a <- roll
  b <- roll
  c <- roll
  let np = (pos + a + b + c) `mod` 10
  pure (np, np + 1 + sc)

tick1 :: M1 ()
tick1 = do
  AState{ turn, player1 = p1, player2 = p2 } <- traceShowId <$> get
  np <- case turn of
    P1 -> takeTurn1 p1
    P2 -> takeTurn1 p2
  modify $ \s -> case turn of
    P1 -> s{ player1 = np, turn = P2 }
    P2 -> s{ player2 = np, turn = P1 }

runTo1 :: Int -> M1 ()
runTo1 n = do
  AState { player1 = (_, sc1), player2 = (_, sc2) } <- get
  if max sc1 sc2 >= 1000
    then pure ()
    else do
      tick1
      runTo1 n

solve1 :: (Int, Int) -> Int
solve1 (p1, p2) =
  let start = AState { die = 0, player1 = (p1, 0), player2 = (p2, 0), turn = P1 }
  in case execState (runTo1 1000) start of
    AState { die, player1 = (_, sc1), player2 = (_, sc2) } -> min sc1 sc2 * die


-- ------
-- Part 2
-- ------

type State2 = (Player, Player, Turn)
type M2 s = ReaderT (HashTable s State2 (Int, Int)) (ST s)

turn2 :: State2 -> Int -> State2
turn2 s n = case s of
  ((pos, sc), p2, P1) ->
    let np = (pos + n) `mod` 10
    in ((np, np + 1 + sc), p2, P2)
  (p1, (pos, sc), P2) ->
    let np = (pos + n) `mod` 10
    in (p1, (np, np + 1 + sc), P1)

dieSearch :: [Int]
dieSearch = [1, 2, 3]

turns :: State2 -> [State2]
turns s = do
  a <- dieSearch
  b <- dieSearch
  c <- dieSearch
  [turn2 s $ a + b + c]

runTo2 :: State2 -> M2 s (Int, Int)
runTo2 s@((_, sc1), (_, sc2), _) = do
  if max sc1 sc2 >= 21
    then pure $ if sc1 > sc2 then (1, 0) else (0, 1)
    else do
      ht <- ask
      cache <- lift $ HT.lookup ht s
      case cache of
        Just wins -> pure wins
        Nothing -> do
          (a1s, b1s) <- unzip <$> traverse runTo2 (turns s)
          let wins = (sum a1s, sum b1s)
          lift $ HT.insert ht s wins
          pure wins

solve2 :: (Int, Int) -> Int
solve2 (p1, p2) = runST $ do
  ht <- HT.new
  let start = ((p1, 0), (p2, 0), P1) :: State2
  (w1, w2) <- runReaderT (runTo2 start) ht
  pure $ max w1 w2

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
