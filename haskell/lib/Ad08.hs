module Ad08 where

import Control.Monad
import Data.List
import Data.Function
import Data.Functor

type Input  = undefined
type Output = undefined

readInput :: IO Input
readInput = readFile "input/08"
  <&> undefined

solve1 :: Input -> Output
solve1 = undefined

solve2 :: Input -> Output
solve2 = undefined

main1 :: IO ()
main1 = readInput
  <&> solve1
  >>= print

main2 :: IO ()
main2 = readInput
  <&> solve2
  >>= print
