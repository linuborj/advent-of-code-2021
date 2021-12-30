{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day09
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP (ReadP)
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List


newtype Chart = Chart (Map (Int, Int) Integer)
  deriving Show

parseDigit :: ReadP Integer
parseDigit = do
  digit <- ReadP.satisfy Char.isDigit
  return $ read [digit]

chart :: [[Integer]] -> Chart
chart = Chart . Map.fromList . concat . zipWith (\y -> zipWith (\x -> ((x, y),)) [0..]) [0..]

parser :: String -> Chart
parser = chart . fst . last . ReadP.readP_to_S (ReadP.many1 parseDigit `ReadP.sepBy1` ReadP.char '\n')

readInput :: IO Chart
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

(!) :: Chart -> (Int, Int) -> Integer
(!) (Chart chart) = (chart Map.!)

member :: (Int, Int) -> Chart -> Bool
member (x, y) (Chart chart) = (x, y) `Map.member` chart

adjacents :: Chart -> (Int, Int) -> [(Int, Int)]
adjacents chart (x, y) = filter (`member` chart) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

isLocalMinima :: Chart -> (Int, Int) -> Bool
isLocalMinima chart (x, y) = all ((> chart ! (x, y)) . (chart !)) . adjacents chart $ (x, y)

localMinimas :: Chart -> [(Int, Int)]
localMinimas chart@(Chart entries) =
  [ (x, y)
  | ((x, y), value) <- Map.toList entries
  , isLocalMinima chart (x, y)
  ]

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map ((+1) . (input !)) . localMinimas $ input

flood :: Chart -> (Int, Int) -> Set (Int, Int)
flood chart = flood' Set.empty
  where
    flood' basin (x, y) = (x, y) `Set.insert` Set.unions
      [ flood' basin' (i, j)
      | (i, j) <- adjacents chart (x, y)
      , chart ! (i, j) < 9
      , chart ! (i, j) >= chart ! (x, y)
      , not $ (i, j) `Set.member` basin
      , let basin' = (i, j) `Set.insert` basin
      ]

basins :: Chart -> [Set (Int, Int)]
basins chart = map (flood chart) . localMinimas $ chart

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  let biggestBasins = take 3 . List.sortOn (negate . length) . basins $ input
  print . product . map length $ biggestBasins

main :: IO ()
main = do
  part1 -- prints 'Part1: 524'
  part2 -- prints 'Part2: 1235430'

