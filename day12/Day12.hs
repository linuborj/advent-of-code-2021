module Main where

import Paths_day12
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Graph (Graph)
import qualified Graph
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Explore

parser :: String -> Graph
parser = fst . last . ReadP.readP_to_S Graph.parse

readInput :: IO Graph
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . length . Explore.paths $ input


part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 4304'
  part2 -- prints 'Part2: '

