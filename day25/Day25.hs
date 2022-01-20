module Main where

import Paths_day25
import Data.Vector.V2 (V2 (..))
import Data.Chart (Chart)
import qualified Data.Chart as Chart


readInput :: IO (Chart Char)
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Chart.fromString $ content

south :: (V2 Int, V2 Int) -> V2 Int -> V2 Int
south (V2 _ yMin, V2 _ yMax) (V2 x y)
  | y + 1 > yMax = V2 x yMin
  | otherwise = V2 x (y+1)

east :: (V2 Int, V2 Int) -> V2 Int -> V2 Int
east (V2 xMin _, V2 xMax _) (V2 x y)
  | x + 1 > xMax = V2 xMin y
  | otherwise = V2 (x+1) y

step :: Char -> (V2 Int -> V2 Int) -> Chart Char -> Chart Char
step cucumber move chart = foldl next chart $ Chart.toList chart
  where
    next chart' (V2 x y, c)
      | c == cucumber && not occupied = Chart.switch (V2 x y) (V2 x' y') chart'
      | otherwise = chart'
      where
        V2 x' y' = move (V2 x y)
        occupied = chart Chart.! V2 x' y' /= '.'

evolve :: (V2 Int, V2 Int) -> Chart Char -> [Chart Char]
evolve bounds = iterate (step 'v' (south bounds) . step '>' (east bounds))

untilGridLocked :: Eq a => [a] -> [a]
untilGridLocked [] = []
untilGridLocked xs = extract . takeWhile (uncurry (/=)) $ zip xs (tail xs)
  where
    extract [] = []
    extract((x,y):xs) = x:y:map snd xs

part1 :: IO ()
part1 = do
  input <- readInput
  let bounds = Chart.dimensions input
  putStr "Part1: "
  print . length . untilGridLocked . evolve bounds $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 528'

