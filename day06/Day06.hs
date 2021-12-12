module Main where

import Paths_day06
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


newtype State = State { unState :: IntMap Integer }
  deriving (Show, Read, Eq)

state :: [Int] -> State
state = State . foldr (\age -> IntMap.insertWith (+) age 1) IntMap.empty

parseInt :: ReadP Int
parseInt = do
  digits <- many1 $ satisfy Char.isDigit
  return $ read digits

parser :: String -> State
parser = state . fst . last . readP_to_S (parseInt `sepBy1` char ',')

readInput :: IO State
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

evolve :: State -> State
evolve = State . IntMap.foldlWithKey evolve' IntMap.empty . unState
  where
    add (age, amount) sea = IntMap.insertWith (+) age amount sea
    evolve' sea age amount
      | age < 0  = error "Illegal state"
      | age == 0 = (8, amount) `add` ((6, amount) `add` sea)
      | otherwise = (age - 1, amount) `add` sea

total :: State -> Integer
total = IntMap.foldl (+) 0 . unState

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . total $ iterate evolve input !! 80

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . total $ iterate evolve input !! 256

main :: IO ()
main = do
  part1 -- prints 'Part1: 377263'
  part2 -- prints 'Part2: 1695929023803'

