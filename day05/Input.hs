module Input
  ( readInput
  ) where

import Paths_day05
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char
import Chart

parseInt :: ReadP Int
parseInt =  do
  digits <- many1 $ satisfy Char.isDigit
  return $ read digits

parseCoordinate :: ReadP Coordinate
parseCoordinate = do
  x <- parseInt
  char ','
  y <- parseInt
  return (x, y)

parseLine :: ReadP Line
parseLine = do
  u <- parseCoordinate
  string " -> "
  v <- parseCoordinate
  return (u, v)

parseInput :: ReadP [Line]
parseInput = parseLine `sepBy1` char '\n'

parser :: String -> [Line]
parser = fst . last . readP_to_S parseInput

readInput :: IO [Line]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return $ parser content

