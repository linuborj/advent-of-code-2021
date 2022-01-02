module Main where

import Paths_day15
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP as ReadP
import Graph (Graph)
import qualified Graph
import qualified Dijkstra
import qualified LeastCostTree
import Data.Functor ((<&>))

grid :: String -> [[Int]]
grid = map (map (read . (:[]))) . lines

readInput :: IO [[Int]]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . grid $ content

part1 :: IO ()
part1 = do
  graph <- readInput <&> Graph.connectedGrid
  let leastCostTree = Dijkstra.leastCostTree graph (0, 0)
  putStr "Part1: "
  print $ LeastCostTree.leastCost leastCostTree (99, 99)

embiggen :: [[Int]] -> [[Int]]
embiggen = foldl1 (zipWith (++)) . grow . foldl1 (++) . grow
  where
    grow = take 5 . iterate (map $ map $ (1 `max`) . (`mod` 10) . (+1))

part2 :: IO ()
part2 = do
  graph <- readInput <&> Graph.connectedGrid . embiggen
  let leastCostTree = Dijkstra.leastCostTree graph (0, 0)
  putStr "Part2: "
  print $ LeastCostTree.leastCost leastCostTree (499, 499)

main :: IO ()
main = do
  part1 -- prints 'Part1: 527'
  part2 -- prints 'Part2: 2887'

