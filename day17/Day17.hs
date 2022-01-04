{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day17
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import qualified Data.List as List


data TargetArea = TargetArea
  { xRange :: (Int, Int)
  , yRange :: (Int, Int)
  } deriving (Show, Eq, Ord)

asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

intParser :: ReadP Int
intParser = foldl1 (ReadP.<++)
  [ ReadP.char '-' *> (negate <$> positive)
  , positive
  ]
  where
    positive = read <$> asManyAsPossible (ReadP.satisfy Char.isDigit)

rangeParser :: ReadP (Int, Int)
rangeParser = do
  from <- intParser
  ReadP.string ".."
  to <- intParser
  return (from, to)

targetAreaParser :: ReadP TargetArea
targetAreaParser = do
  ReadP.string "target area: x="
  xRange <- rangeParser
  ReadP.string ", y="
  yRange <- rangeParser
  ReadP.skipSpaces 
  ReadP.eof
  return $ TargetArea xRange yRange

parser :: String -> TargetArea
parser input = case targetAreaParser `ReadP.readP_to_S` input of
  [(targetArea, "")] -> targetArea
  _                  -> error "could not parse target area"

readInput :: IO TargetArea
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

triangleNumber :: Int -> Int
triangleNumber n = (n^2 + n) `div` 2

-- To simplify things we'll assume that the target area is somewhere in x=0,1,2...
horizontalSpeedRange :: TargetArea -> (Int, Int)
horizontalSpeedRange TargetArea { xRange = (xMin, xMax) } = (dxMin, dxMax)
  where
    -- the projectile speed is reduced by 1 each turn, as such the most it can travel before its speed is 0 is given by
    -- \sum_{n=1}^{dx} n, or simply `triangle dx`.
    dxMin = last . takeWhile ((<= xMin) . triangleNumber) $ [1..] 
    -- if dx > xMax then the projectile will overshoot the target in the first step
    dxMax = xMax

-- To simplify things we'll assume that the target area is somewhere in y=0,-1,-2...
verticalSpeedRange :: TargetArea -> (Int, Int)
verticalSpeedRange TargetArea { yRange = (yMin, yMax) } = (yMin, abs yMin)
  where
    -- if dy < yMin then the projectile will overshoot the target in the first step
    dyMin = yMin
    -- the projectile speed is symmetric in the sense that if it is fired from y=0 with speed dy>0 then
    -- after n steps its position will be y=0 again and its speed will be -dy. As such if the
    -- speed is greater the -yMin the projectile will overshoot the target at the n+1 step.
    dyMax = -yMin

velocities :: TargetArea -> [(Int, Int)]
velocities targetArea = [ (dx, dy) | dy <- [dyMax, dyMax-1 .. dyMin], dx <- [dxMin .. dxMax]]
  where
    (dyMin, dyMax) = verticalSpeedRange targetArea
    (dxMin, dxMax) = horizontalSpeedRange targetArea

positions :: (Int, Int) -> [(Int, Int)]
positions = map fst . iterate step . ((0, 0),)
  where
    step ((x, y), (dx, dy)) = ((x', y'), (dx', dy'))
      where
        x'  = x + dx
        y'  = y + dy
        dx' = max 0 (dx - 1)
        dy' = dy - 1

data Status = InFlight | Hit | OutOfBounds
  deriving (Show, Eq, Ord)

status :: TargetArea -> (Int, Int) -> Status
status TargetArea { xRange = (xMin, xMax), yRange = (yMin, yMax) } (x, y)
  | x > xMax || y < yMin = OutOfBounds
  | x >= xMin && x <= xMax && y >= yMin && y <= yMax = Hit
  | otherwise = InFlight

hits :: TargetArea -> [(Int, Int)]
hits targetArea = filter hit . velocities $ targetArea
  where
    hit velocity = any ((== Hit) . status targetArea) . takeWhile ((/= OutOfBounds) . status targetArea) . positions $ velocity

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  -- The maximum y is given by dy + (dy-1) + (dy-2) + .. + 1, or simply `triangle dy`
  print . triangleNumber . snd . last . List.sortOn snd . hits $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . length . hits $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 19503'
  part2 -- prints 'Part2: 5200'

