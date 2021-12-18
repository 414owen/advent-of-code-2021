module Ad18 (main1, main2) where

import Data.Char
import Data.Functor

data Tree = Pair Tree Tree | Leaf Int
data Op = Open | Close | Regular Int
type Number = [Op]

readNumber :: String -> (Number, String)
readNumber ('[' : xs) =
  let (n, xs') = readNumber xs
      (m, xs'') = readNumber (tail xs')
  in (Open : (n <> m <> [Close]), tail xs'')
readNumber xs =
  let (xs', xs'') = span isDigit xs
  in ([Regular $ read xs'], xs'')

readInput :: IO [Number]
readInput = readFile "input/18"
  <&> lines
  <&> filter (/= "")
  <&> fmap readNumber
  <&> fmap fst

addR :: Int -> Number -> Number
addR n (Regular r : xs) = Regular (n + r) : xs
addR n (x : xs) = x : addR n xs
addR _ [] = []

simplE :: Int -> Number -> (Number, Int, Bool)
simplE depth (Open : Regular a : Regular b : Close : xs)
  | depth >= 4 = (Regular 0 : addR b xs, a, True)
simplE depth (Open : xs) = case simplE (depth + 1) xs of
  (rest, l, t) -> (Open : rest, l, t)
simplE depth (Close : xs) = case simplE (depth - 1) xs of
  (rest, l, t) -> (Close : rest, l, t)
simplE depth (Regular r : xs) =
  let (rest, n, t) = simplE depth xs
  in (Regular (r + n) : rest, 0, t)
simplE _ [] = ([], 0, False)

simplS :: Number -> (Number, Bool)
simplS (Regular n : xs)
  | n >= 10 =
      ( [ Open
        , Regular (n `div` 2)
        , Regular $ (n `div` 2) + (n `mod` 2)
        , Close
        ] <> xs
      , True)
simplS (x:xs) = case simplS xs of
  (rest, t) -> (x : rest, t)
simplS [] = ([], False)

simpl :: Number -> Number
simpl n = case simplE 0 n of
  (m, _, True) -> simpl m
  (m, _, False) -> case simplS m of
    (o, True) -> simpl o
    (o, False) -> o

addNum :: Number -> Number -> Number
addNum a b = simpl $ [Open] <> a <> b <> [Close]

toTree :: Number -> (Tree, Number)
toTree (Open : xs) = let (l, xs') = toTree xs
                         (r, xs'') = toTree xs'
                     in (Pair l r, tail xs'')
toTree (Regular r : xs) = (Leaf r, xs)
toTree _ = error "can't convert number to tree"

mag :: Tree -> Int
mag (Leaf n) = n
mag (Pair l r) = 3 * mag l + 2 * mag r

solve1 :: [Number] -> Int
solve1 (x:xs) = mag $ fst $ toTree $ foldl addNum x xs
solve1 _ = error "Bad input"

twoMags :: [Number] -> [Int]
twoMags xs = do
  a <- xs
  b <- xs
  pure $ mag $ fst $ toTree $ addNum a b

solve2 :: [Number] -> Int
solve2 = maximum . twoMags

main1 :: IO ()
main1 = readInput >>= print . solve1

main2 :: IO ()
main2 = readInput >>= print . solve2
