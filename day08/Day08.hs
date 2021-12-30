module Main where

import Paths_day08
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Signal (Signal)
import qualified Signal
import Data.Set (Set)
import qualified Data.Set as Set
import Encoder (Encoder)
import qualified Encoder
import Digit (Digit)
import qualified Digit

parsePattern :: ReadP [Signal]
parsePattern = ReadP.many1 Signal.parse

data Example = Example
  { patterns :: [Set Signal]
  , output :: [Set Signal]
  } deriving Show

parseExample :: ReadP Example
parseExample = do
  patterns <- parsePattern `ReadP.sepBy1` ReadP.char ' '
  ReadP.string " | "
  output <- parsePattern `ReadP.sepBy1` ReadP.char ' '
  return $ Example (map Set.fromList patterns) (map Set.fromList output)
  where
    parsePatterns = ReadP.many1 Signal.parse

parseExamples :: ReadP [Example]
parseExamples = do
  examples <- parseExample `ReadP.sepBy1` ReadP.char '\n'
  ReadP.char '\n'
  ReadP.eof
  return examples

parser :: String -> [Example]
parser = fst . last . ReadP.readP_to_S parseExamples

readInput :: IO [Example]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  let outputs = concatMap output input
      wanted  = filter (\x -> length x  `elem` [2, 4, 3, 7]) outputs
  putStr "Part1: "
  print . length $ wanted

decode :: Encoder -> Set Signal -> Digit
decode encoder pattern = head $ filter ((== pattern) . Encoder.encode encoder) [Digit.Zero .. Digit.Nine]

number :: [Digit] -> Integer
number = sum . zipWith (*) [10^n | n <- [0..]] . map Digit.integer . reverse

part2' :: Example -> Integer
part2' (Example patterns output) = number decoded
  where
    encoder = Encoder.encoder patterns
    decoded = map (decode encoder) output

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . sum . map part2' $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 390'
  part2 -- prints 'Part2: 1011785'

