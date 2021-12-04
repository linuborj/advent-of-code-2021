module Main where

import Paths_day01


readInput :: IO [Int]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . map read . lines $ content


comparisons :: [Int] -> [Ordering]
comparisons xs = zipWith compare (tail xs) xs

part1 :: IO ()
part1 = do
  input <- readInput
  let increases = length . filter (== GT) . comparisons $ input
  putStr "Part1: "
  print increases


main :: IO ()
main = do
  part1 -- prints `Part1: 1553`

