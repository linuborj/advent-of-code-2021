module Main where

import Paths_day06
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char


parseInt :: ReadP Int
parseInt = do
  digits <- many1 $ satisfy Char.isDigit
  return $ read digits

parser :: String -> [Int]
parser = fst . last . readP_to_S (parseInt `sepBy1` char ',')

readInput :: IO [Int]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

evolve' :: Int -> [Int]
evolve' x
  | x < 0     = error "Illegal state"
  | x == 0    = [8, 6]
  | otherwise = [x-1]

evolve :: [Int] -> [Int]
evolve = concatMap evolve'

part1 :: IO ()
part1 = do
  state0 <- readInput
  putStr "Part1: "
  print . length $ iterate evolve state0 !! 80

part2 :: IO ()
part2 = do
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 377263'
  part2 -- prints 'Part2: '

