module Main where

import Paths_day02
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char

data Direction = Forward | Up | Down
  deriving (Show, Read, Eq)

data Movement = Movement
  { direction :: Direction
  , amount :: Int
  } deriving (Show, Read, Eq)

parseInt :: ReadP Int
parseInt = do
  digits <- many1 $ satisfy Char.isDigit
  return $ read digits

parseDirection :: ReadP Direction
parseDirection = choice
  [ string "forward" >> return Forward
  , string "up" >> return Up
  , string "down" >> return Down
  ]

parseMovement :: ReadP Movement
parseMovement = do
  direction <- parseDirection
  string " "
  amount <- parseInt
  return Movement
    { direction = direction
    , amount = amount
    }

parser :: String -> [Movement]
parser = fst . last . readP_to_S (parseMovement `sepBy1` char '\n')

readInput :: IO [Movement]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

move :: (Int, Int) -> Movement -> (Int, Int)
move (x, y) = \case
  Movement Forward n -> (x + n, y)
  Movement Up n      -> (x, y - n)
  Movement Down n    -> (x, y + n)

part1 :: IO ()
part1 = do
  movements <- readInput
  let position = foldl move (0, 0) movements
  putStr "Part1: "
  print $ uncurry (*) position

move' :: ((Int, Int), Int) -> Movement -> ((Int, Int), Int)
move' ((x, y), d) = \case
  Movement Forward n -> ((x + n, y + d * n), d)
  Movement Up n      -> ((x, y), d - n)
  Movement Down n    -> ((x, y), d + n)

part2 :: IO ()
part2 = do
  movements <- readInput
  let position = foldl move' ((0, 0), 0) movements
  putStr "Part2: "
  print . uncurry (*) . fst $ position

main :: IO ()
main = do
  part1 -- prints 'Part1: 1924923'
  part2 -- prints 'Part1: 1982495697'

