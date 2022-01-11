{-# LANGUAGE FlexibleInstances #-}
module Main where

import Paths_day22
import Prelude hiding (length)
import qualified Data.Maybe as Maybe
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char


data Range = Range Int Int
  deriving (Show, Eq, Ord)

length :: Range -> Int
length (Range from to) = to - from + 1

overlap :: Range -> Range -> Maybe Range
overlap (Range i j) (Range k l)
  | j < k || i > l = Nothing                         -- i--j  k--l / k--l i--j
  | i >= k && j <= l = Just $ Range i j              -- k--i--j--l
  | i <= k && j >= l = Just $ Range k l              -- i--k--l--j
  | otherwise = Just $ Range (i `max` k) (j `min` l) -- i--k--j--l / k--i--l--j

asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

intParser :: ReadP Int
intParser = foldl1 (ReadP.<++)
  [ negate <$> (ReadP.char '-' *> positive)
  , positive
  ]
  where
    positive = read <$> asManyAsPossible (ReadP.satisfy Char.isDigit)

rangeParser :: ReadP Range
rangeParser = do
  from <- intParser
  ReadP.string ".."
  to <- intParser
  return $ Range (from `min` to) (from `max` to)

data Cuboid = Cuboid Range Range Range
  deriving (Show, Eq, Ord)
  
volume :: Cuboid -> Int
volume (Cuboid x y z) = product $ map length [x, y, z]

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection (Cuboid x y z) (Cuboid x' y' z') = Cuboid <$> x `overlap` x' <*> y `overlap` y' <*> z `overlap` z'

cuboidParser :: ReadP Cuboid
cuboidParser = Cuboid <$> (ReadP.string "x=" *> rangeParser) <*> (ReadP.string ",y=" *> rangeParser) <*> (ReadP.string ",z=" *> rangeParser)

data Mode = On | Off
  deriving (Show, Eq, Ord)

invert :: Mode -> Mode
invert Off = On
invert On = Off

modeParser :: ReadP Mode
modeParser = foldl1 (ReadP.<++)
  [ ReadP.string "on" >> return On
  , ReadP.string "off" >> return Off
  ]

data Instruction = Instruction Mode Cuboid
  deriving (Show, Eq, Ord)

instructionParser :: ReadP Instruction
instructionParser = Instruction <$> modeParser <* ReadP.char ' ' <*> cuboidParser

data Change = Change Mode Cuboid
  deriving (Show, Eq, Ord)

newtype State = State { unState :: [Change] }

apply :: State -> Instruction -> State
apply (State state) (Instruction mode cuboid) = State $ case mode of
  On -> Change On cuboid:state'
  Off -> state'
  where
    state' = Maybe.mapMaybe invertIntersectedArea state ++ state
    invertIntersectedArea (Change mode' cuboid') = Change (invert mode') <$> cuboid' `intersection` cuboid

turnedOnCubesAfter :: [Instruction] -> Int
turnedOnCubesAfter = sum . map delta . unState . foldl apply (State [])
  where
    delta (Change mode cuboid) = case mode of
      On -> volume cuboid
      Off -> negate $ volume cuboid

initializationRegion :: Cuboid
initializationRegion = Cuboid (Range (-50) 50) (Range (-50) 50) (Range (-50) 50)

intersects :: Instruction -> Cuboid -> Bool
intersects (Instruction _ cuboid) cuboid' = Maybe.isJust $ cuboid `intersection` cuboid'

separatedBy :: ReadP a -> ReadP b -> ReadP [a]
separatedBy parser separator = foldl1 (ReadP.<++)
  [ (:) <$> (parser <* separator) <*> separatedBy parser separator
  , (:[]) <$> parser
  , return []
  ]

inputParser :: ReadP [Instruction]
inputParser = instructionParser `separatedBy` ReadP.char '\n'

parser :: String -> [Instruction]
parser input = case ReadP.readP_to_S inputParser input of 
  [(parsed, "")] -> parsed
  _ -> error "could not parse"

readInput :: IO [Instruction]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . turnedOnCubesAfter . filter (`intersects` initializationRegion) $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . turnedOnCubesAfter $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 587097'
  part2 -- prints 'Part2: 1359673068597669'

