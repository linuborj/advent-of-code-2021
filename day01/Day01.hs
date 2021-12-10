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

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length xs < n = []
  | otherwise     = take n xs : slidingWindow n (tail xs)

part2 :: IO ()
part2 = do
  input <- readInput
  let increases = length . filter (== GT) . comparisons . map sum . slidingWindow 3 $ input
  putStr "Part2: "
  print increases

main :: IO ()
main = do
  part1 -- prints `Part1: 1553`
  part2 -- prints `Part2: 1597`

