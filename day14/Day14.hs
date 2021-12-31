{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day14
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import State (State, Statistics (..) )
import qualified State


parser :: String -> State
parser = fst . last . ReadP.readP_to_S State.parse

readInput :: IO State
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return .parser $ content

part1 :: IO ()
part1 = do
  state <- readInput
  let state' = State.evolve state !! 10
  let Statistics { leastCommonElement, mostCommonElement } = State.statistics state'
  putStr "Part1: "
  print $ snd mostCommonElement - snd leastCommonElement

part2 :: IO ()
part2 = do
  state <- readInput
  let state' = State.evolve state !! 40
  let Statistics { leastCommonElement, mostCommonElement } = State.statistics state'
  putStr "Part2: "
  print $ snd mostCommonElement - snd leastCommonElement

main :: IO ()
main = do
  part1 -- prints 'Part1: 3284'
  part2 -- prints 'Part2: 4302675529689'

