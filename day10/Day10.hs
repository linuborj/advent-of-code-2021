module Main where

import Paths_day10
import qualified Data.Maybe as Maybe


opens :: Char -> Bool
opens = (`elem` "<({[")

invert :: Char -> Char
invert '>' = '<'
invert ')' = '('
invert '}' = '{'
invert ']' = '['
invert c   = error $ "'" ++ [c] ++ "' is not a closing bracket"

score :: Char -> Integer
score '>' = 25137
score ')' = 3
score '}' = 1197
score ']' = 57
score c   = error $ "'" ++ [c] ++ "' is not a closing bracket"

firstIllegalCharacter :: String -> Maybe Char
firstIllegalCharacter = illegal ""
  where
    illegal "" "" = Nothing
    illegal "" (x:xs)
      | opens x   = illegal [invert x] xs
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
  print . sum . map score . Maybe.mapMaybe firstIllegalCharacter $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints 'Part1: 370407'
  part2 -- prints 'Part2: '

