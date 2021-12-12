module Main where

import Paths_day03
import Text.ParserCombinators.ReadP
import qualified Data.Char as Char
import qualified Data.List as List


data Bit = Zero | One
  deriving (Show, Eq)

decimal :: [Bit] -> Int
decimal = sum . zipWith (*) (map (2^) [0..]) . map f . reverse
  where
    f Zero = 0
    f One  = 1


data Statistics = Statistics
  { zeroes :: Int
  , ones :: Int
  } deriving (Show, Read, Eq)

statistics0 :: Statistics
statistics0 = Statistics 0 0

mostCommonBit :: Statistics -> Bit
mostCommonBit (Statistics zeroes ones)
  | zeroes > ones = Zero
  | zeroes < ones = One
  | otherwise     = One

leastCommonBit :: Statistics -> Bit
leastCommonBit (Statistics zeroes ones)
  | zeroes < ones = Zero
  | zeroes > ones = One
  | otherwise     = Zero

updateStatistics :: Statistics -> Bit -> Statistics
updateStatistics (Statistics zeroes ones) Zero = Statistics (zeroes + 1) ones
updateStatistics (Statistics zeroes ones) One  = Statistics zeroes (ones + 1)


parseBits :: ReadP [Bit]
parseBits = do
  bits <- many1 $ satisfy (`elem` "01") 
  return $ map (\c -> if c == '1' then One else Zero) bits

parser :: String -> [[Bit]]
parser = fst . last . readP_to_S (parseBits `sepBy1` char '\n')

readInput :: IO [[Bit]]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content


statistics :: [[Bit]] -> [Statistics]
statistics = foldl (zipWith updateStatistics) (repeat statistics0)


part1 :: IO ()
part1 = do
  rows <- readInput
  let gammaRate   = decimal . map mostCommonBit  . statistics $ rows
      epsilonRate = decimal . map leastCommonBit . statistics $ rows
  putStr "Part1: "
  print $ gammaRate * epsilonRate


matchCriteria :: (Statistics -> Bit) -> [[Bit]] -> [Bit]
matchCriteria _        [x] = x
matchCriteria criteria xs  = bit : matchCriteria criteria xs'
  where
    bit = criteria . head . statistics $ xs
    xs' = map tail . filter ((== bit) . head) $ xs

part2 :: IO ()
part2 = do
  rows <- readInput
  let oxygenRating = decimal $ matchCriteria mostCommonBit rows
      cO2Rating    = decimal $ matchCriteria leastCommonBit rows
  putStr "Part2: "
  print $ oxygenRating * cO2Rating


main :: IO ()
main = do
  part1 -- prints 'Part1: 2954600'
  part2 -- prints 'Part2: 1662846'

