module Main where

import Paths_day01


data Change = Decrease | Unchanged | Increase
  deriving (Show, Eq)

change x y
  | x < y  = Increase
  | x == y = Unchanged
  | x > y  = Decrease

changes :: [Int] -> [Change]
changes xs = zipWith change xs (tail xs)

readInput :: IO [Int]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . map read . lines $ content

part1 :: IO ()
part1 = do
  input <- readInput
  let increases = length . filter (== Increase) . changes $ input
  putStr "Part1: "
  print increases

main :: IO ()
main = do
  part1 -- prints `Part1: 1553`

