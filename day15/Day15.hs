{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day15
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP as ReadP
import Graph (Graph)
import qualified Graph
import qualified Dijkstra
import qualified LeastCostTree

grid :: String -> [[Integer]]
grid = map (map (read . (:[]))) . lines

readInput :: IO (Graph (Integer, Integer) Integer)
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Graph.connectedGrid . grid $ content

part1 :: IO ()
part1 = do
  graph <- readInput
  let leastCostTree = Dijkstra.leastCostTree graph (0, 0)
  putStr "Part1: "
  print $ LeastCostTree.leastCost leastCostTree (99, 99)

part2 :: IO ()
part2 = do
  graph <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 527'
  part2 -- prints 'Part2: '

