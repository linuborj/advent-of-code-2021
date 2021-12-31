{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day14
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List


data Input = Input
  { rules :: Map String Char
  , template :: String
  } deriving Show

parseRule :: ReadP (String, Char)
parseRule = do
  x <- ReadP.satisfy Char.isAsciiUpper
  y <- ReadP.satisfy Char.isAsciiUpper
  ReadP.string " -> "
  char <- ReadP.satisfy Char.isAsciiUpper
  return ([x, y], char)

parseRules :: ReadP (Map String Char)
parseRules = do
  rules <- parseRule `ReadP.sepBy1` ReadP.char '\n'
  return $ Map.fromList rules

parseInput :: ReadP Input
parseInput = do
  template <- ReadP.many1 $ ReadP.satisfy Char.isAsciiUpper
  ReadP.string "\n\n"
  rules <- parseRules
  return $ Input
    { rules
    , template
    }

parser :: String -> Input
parser = fst . last . ReadP.readP_to_S parseInput

readInput :: IO Input
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return .parser $ content

step :: Map String Char -> String -> String
step _     []       = []
step _     [x]      = [x]
step rules (x:y:xs) = case rules Map.!? [x,y] of
  Nothing -> x : step rules (y:xs)
  Just c  -> x : c : step rules (y:xs)

data Statistics = Statistics
  { most  :: Int
  , least :: Int
  } deriving Show

statistics :: String -> Statistics
statistics template = Statistics
  { most  = maximum counts
  , least = minimum counts
  }
  where
    counts = foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty template

part1 :: IO ()
part1 = do
  Input { rules, template } <- readInput
  let template' = iterate (step rules) template !! 10
  let Statistics { least, most } = statistics template'
  putStr "Part1: "
  print $ most - least

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 370407'
  part2 -- prints 'Part2: 3249889609'

