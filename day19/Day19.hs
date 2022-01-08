module Main where

import Paths_day19
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Scanner (Scanner)
import qualified Scanner
import Network (Network (..))
import qualified Network
import qualified PointCloud
import qualified Data.Maybe as Maybe
import qualified Data.List as List


inputParser :: ReadP [Scanner]
inputParser = foldl1 (ReadP.<++)
  [ (:) <$> Scanner.parser <* ReadP.char '\n' <*> inputParser
  , (:[]) <$> Scanner.parser
  , return []
  ]

parse :: ReadP a -> String -> a
parse parser input = case ReadP.readP_to_S parser input of
  [(parsed, "")] -> parsed
  rest           -> error $ "could not parse input: " ++ show (map snd rest)

readInput :: IO [Scanner]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parse inputParser $ content

part1 :: Network -> IO ()
part1 network = do
  putStr "Part1: "
  print . PointCloud.size . Network.beacons $ network

maximumScannerManhattanDistance :: Network -> Int
maximumScannerManhattanDistance network = maximum
  [ sum . abs $ x - y
  | (x, ys) <- zip positions (tail $ List.tails positions)
  , y <- ys
  ]
  where
    positions = Network.scannerPositions network

part2 :: Network -> IO ()
part2 network = do
  putStr "Part2: "
  print . maximumScannerManhattanDistance $ network

main :: IO ()
main = do
  network <- Maybe.fromJust . Network.fromList <$> readInput
  part1 network -- prints 'Part1: 400'
  part2 network -- prints 'Part2: 12168'

