module Main where

import Input (readInput)
import Chart
import Data.Functor ((<&>))


part1 :: IO ()
part1 = do
  lines <- readInput <&> filter (\l -> isHorizontal l || isVertical l)
  let chart = foldl registerLine emptyChart lines
      points = chart `pointsWhere` (>1)
  putStr "Part1: "
  print $ length points

part2 :: IO ()
part2 = do
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 5145'
  part2 -- prints 'Part2: '

