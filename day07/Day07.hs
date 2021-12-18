module Main where

import Paths_day07
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char
import State (State, state, CostFunction, costFunction)
import Cost (fuelCosts)

parseInt :: ReadP Int
parseInt = do
  digits <- many1 $ satisfy Char.isDigit
  return $ read digits

parser :: String -> State
parser = state . fst . last . readP_to_S (parseInt `sepBy1` char ',')

readInput :: IO State
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  let f = costFunction id
  putStr "Part1: "
  print . minimum $ fuelCosts f input

part2 :: IO ()
part2 = do
  putStr "Part2: "


main :: IO ()
main = do
  part1 -- prints 'Part1: 343441'
  part2 -- prints 'Part2: '

