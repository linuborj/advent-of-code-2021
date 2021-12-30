module Main where

import Paths_day10
import qualified Data.Maybe as Maybe
import qualified Data.List as List


opens :: Char -> Bool
opens = (`elem` "<({[")

invert :: Char -> Char
invert '>' = '<'
invert ')' = '('
invert '}' = '{'
invert ']' = '['
invert c   = error $ "'" ++ [c] ++ "' is not a closing bracket"

syntaxErrorScore :: Char -> Integer
syntaxErrorScore '>' = 25137
syntaxErrorScore ')' = 3
syntaxErrorScore '}' = 1197
syntaxErrorScore ']' = 57
syntaxErrorScore c   = error $ "'" ++ [c] ++ "' is not a closing bracket"

firstIllegalCharacter :: String -> Maybe Char
firstIllegalCharacter = illegal ""
  where
    illegal "" "" = Nothing
    illegal "" (x:xs)
      | opens x   = illegal [x] xs
      | otherwise = Just x
    illegal _ "" = Nothing
    illegal (c:cs) (x:xs)
      | opens x       = illegal (x:c:cs) xs
      | invert x == c = illegal cs xs
      | otherwise     = Just x

readInput :: IO [String]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . lines $ content

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map syntaxErrorScore . Maybe.mapMaybe firstIllegalCharacter $ input

autoCompletionScore :: String -> Integer
autoCompletionScore = foldl (\total c -> total * 5 + score c) 0
  where
    score '<' = 4
    score '(' = 1
    score '{' = 3
    score '[' = 2
    score c   = error $ "'" ++ [c] ++ "' is not an opening bracket"

incomplete :: String -> Maybe String
incomplete = incomplete' ""
  where
    incomplete' "" "" = Nothing
    incomplete' "" (x:xs)
      | opens x = incomplete' [x] xs
      | otherwise = Nothing
    incomplete' cs "" = Just cs
    incomplete' (c:cs) (x:xs)
      | opens x       = incomplete' (x:c:cs) xs
      | invert x == c = incomplete' cs xs
      | otherwise     = Nothing

middle :: Ord a => [a] -> a 
middle xs = xs' !! (length xs `div` 2)
  where
    xs' = List.sort xs

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . middle . map autoCompletionScore . Maybe.mapMaybe incomplete $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 370407'
  part2 -- prints 'Part2: 3249889609'

