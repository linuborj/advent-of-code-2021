{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day11
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP (ReadP)
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type State = Map (Int, Int) Int

parseDigit :: ReadP Int
parseDigit = do
  digit <- ReadP.satisfy Char.isDigit
  return $ read [digit]

state :: [[Int]] -> State
state = Map.fromList . concat . zipWith (\y -> zipWith (\x -> ((x, y),)) [0..]) [0..]

parser :: String -> State
parser = state . fst . last . ReadP.readP_to_S (ReadP.many1 parseDigit `ReadP.sepBy1` ReadP.char '\n')

readInput :: IO State
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

adjacents :: State -> (Int, Int) -> [(Int, Int)]
adjacents state (x, y) =
  [ (i, j)
  | i <- [x-1 .. x+1]
  , j <- [y-1 .. y+1]
  , (x, y) /= (i, j)
  , (i, j) `Map.member` state
  ]

increaseOctopuses :: State -> State
increaseOctopuses = Map.map (+1)

flashOctopuses :: (State, Set (Int, Int))-> (State, Set (Int, Int))
flashOctopuses (octopuses, hasAlreadyFlashed)
  | null toFlash = (octopuses, hasAlreadyFlashed)
  | otherwise    = flashOctopuses (increasedOctopuses, toFlash `Set.union` hasAlreadyFlashed)
  where
    mightFlash = Map.keysSet . Map.filter (> 9) $ octopuses
    toFlash = mightFlash Set.\\ hasAlreadyFlashed
    toIncrease = concatMap (adjacents octopuses) toFlash
    increasedOctopuses = foldr (Map.adjust (+1)) octopuses toIncrease

resetOctopuses :: State -> Set (Int, Int) -> (State, Set (Int, Int))
resetOctopuses octopuses flashed = (foldr (`Map.insert` 0) octopuses flashed, flashed)

step :: State -> (State, Set (Int, Int))
step = uncurry resetOctopuses . flashOctopuses . (, Set.empty) . increaseOctopuses

evolve :: State -> [Set (Int, Int)]
evolve state = flashed : evolve state'
  where
    (state', flashed) = step state

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map length . take 100 . evolve $ input

part2 :: IO ()
part2 = do
  input <- readInput
  let keys = Map.keysSet input
  putStr "Part2: "
  print . fst . head . filter ((keys ==) . snd) . zip [1..] . evolve $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 1594'
  part2 -- prints 'Part2: 437'

