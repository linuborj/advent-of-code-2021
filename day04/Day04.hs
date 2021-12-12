module Main where

import Input (readInput)
import qualified Number
import qualified Board
import Board (Board)

newtype Game = Game { boards :: [Board.Board] }
  deriving (Show, Eq)

mark :: Game -> Int -> Game
mark (Game boards) n = Game $ map (`Board.mark` n) boards

winner :: Game -> Maybe Board
winner (Game boards) = case filter Board.isWinner boards of
  []  -> Nothing
  [x] -> Just x
  _   -> error "Only one boards can win at a time"

winningScore :: Game -> Int -> Maybe Int
winningScore game n = do
  board <- winner game
  board `Board.score` n

firstWinningScore :: Game -> [Int] -> Maybe Int
firstWinningScore (Game boards) [] = Nothing
firstWinningScore game (x:xs) = maybe nextPlay return score
  where
    game' = game `mark` x
    score = winningScore game' x
    nextPlay = firstWinningScore game' xs


part1 :: IO ()
part1 = do
  putStr "Part1: "
  (draws, boards) <- readInput
  print $ firstWinningScore (Game boards) draws


part2 :: IO ()
part2 = do
  putStr "Part2: "


main :: IO ()
main = do
  (draws, boards) <- readInput
  part1 -- prints 'Part1: 10374'
  part2 -- prints 'Part2: '

