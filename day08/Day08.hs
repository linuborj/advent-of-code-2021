module Main where

import Paths_day08
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char

data Example = Example
  { patterns :: [String]
  , output   :: [String]
  } deriving (Show, Eq)

parseWord :: ReadP String
parseWord = many1 $ satisfy Char.isAsciiLower

parseWords :: ReadP [String]
parseWords = parseWord `sepBy1` char ' '

parseExample :: ReadP Example
parseExample = do
  patterns <- parseWords
  string " | "
  output <- parseWords
  return Example
    { patterns = patterns
    , output = output
    }

parser :: String -> [Example]
parser = fst . last . readP_to_S (parseExample `sepBy1` char '\n')

readInput :: IO [Example]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  let outputs = concatMap output input
      wanted  = filter (\x -> length x  `elem` [2, 4, 3, 7]) outputs
  putStr "Part1: "
  print . length $ wanted

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: '
  part2 -- prints 'Part2: '

