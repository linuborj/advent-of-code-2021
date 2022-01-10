{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day21
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Player (Player)
import qualified Player
import Deterministic (Result (..))
import qualified Deterministic
import qualified Dirac


inputParser :: ReadP (Player, Player)
inputParser = (,) <$> Player.parser <*> Player.parser

parser :: ReadP a -> String -> a
parser p s = case ReadP.readP_to_S p s of
  [(parsed, "")] -> parsed
  _ -> error "could not parse"

readInput :: IO (Player, Player)
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser inputParser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  let Result { loser, lastRoll } = Deterministic.play input
  putStr "Part1: "
  print $ Player.score loser * Deterministic.number lastRoll

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print $ uncurry max . uncurry Dirac.wins $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 916083'
  part2 -- prints 'Part2: 49982165861983'

