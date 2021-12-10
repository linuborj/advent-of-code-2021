module Main where

import Paths_day03
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char

parseDigits :: Read a => ReadP [a]
parseDigits = do
  digits <- many1 $ satisfy Char.isDigit
  return $ map (read . (:[])) digits

parser :: Read a => String -> [[a]]
parser = fst . last . readP_to_S (parseDigits `sepBy1` char '\n')

readInput :: IO [[Int]]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

decimal :: [Bool] -> Int
decimal = sum . zipWith (*) (map (2^) [0..]) . reverse . map (\x -> if x then 1 else 0)

part1 :: IO ()
part1 = do
  rows <- readInput
  let sums = foldl1 (zipWith (+)) rows
      gamma = map (< length rows `div` 2) sums
      epsilon = map not gamma
  putStr "Part1: "
  print $ decimal gamma * decimal epsilon

main :: IO ()
main =
  part1 -- prints 'Part1: 2954600'

