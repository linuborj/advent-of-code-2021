module Input
  ( readInput
  ) where

import Paths_day04
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char
import Number
import Board

data Input = Input
  { draws :: [Int]
  , boards :: [Board]
  } deriving Show

parseNumber :: ReadP Number
parseNumber = do
  number <- many1 $ satisfy Char.isDigit
  return $ Unmarked (read number)

parseBoardRow :: ReadP [Number]
parseBoardRow = parseNumber' `sepBy1` numberSeparator
  where
    numberSeparator = char ' '
    parseNumber' = foldl1 (<++)
      [ parseNumber
      , char ' ' >> parseNumber
      ]

parseBoard :: ReadP Board
parseBoard = do
  rows <- parseBoardRow `sepBy1` char '\n'
  return $ Board rows

parseDraws :: ReadP [Int]
parseDraws = parseDraw `sepBy1` char ','
  where
    parseDraw = do
      digits <- many1 $ satisfy Char.isDigit
      return $ read digits

parseInput :: ReadP Input
parseInput = do
  draws <- parseDraws
  munch1 (== '\n')
  boards <- parseBoard `sepBy1` string "\n\n"
  munch (== '\n')
  return $ Input draws boards

parser :: String -> Input
parser = fst . last . readP_to_S parseInput

readInput :: IO ([Int], [Board])
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  let Input draws boards = parser content
  return (draws, boards)

