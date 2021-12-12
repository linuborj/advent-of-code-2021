{-# LANGUAGE TupleSections #-}
module Main where

import Input (readInput)
import qualified Number
import qualified Board
import Board (Board)
import qualified Data.Maybe as Maybe

newtype Game = Game { boards :: [Board.Board] }
  deriving (Show, Eq)

mark :: Game -> Int -> Game
mark (Game boards) n = Game $ map (`Board.mark` n) boards

extractWinners :: Game -> (Game, [Board])
extractWinners (Game boards) = (Game nonWinners, winners)
  where
    nonWinners = filter (not . Board.isWinner) boards
    winners = filter Board.isWinner boards

winners :: Game -> [Int] -> [Int]
winners game [] = []
winners game (x:xs) = scores ++ winners game' xs
  where
    (game', boards) = extractWinners $ game `mark` x
    scores = Maybe.mapMaybe (`Board.score` x) boards

part1 :: IO ()
part1 = do
  (draws, boards) <- readInput
  putStr "Part1: "
  print . head . winners (Game boards) $ draws

lastWinningScore :: Game -> [Int] -> Maybe Int
lastWinningScore game = undefined

part2 :: IO ()
part2 = do
  (draws, boards) <- readInput
  putStr "Part2: "
  print . last . winners (Game boards) $ draws

main :: IO ()
main = do
  (draws, boards) <- readInput
  part1 -- prints 'Part1: 10374'
  part2 -- prints 'Part2: '

