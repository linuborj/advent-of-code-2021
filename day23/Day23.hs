module Main where

import Paths_day23
import qualified Data.Maybe as Maybe
import Burrow (Burrow)
import qualified Burrow
import Data.Graph.Search (aStar, Step (..), VisitedNode (..))


step :: Burrow -> [Step Burrow Int]
step burrow =
  [ Step burrow' cost (Burrow.minimumEnergyToOrganizeBurrow burrow')
  | (burrow', cost) <- Burrow.step burrow
  ]

leastEnergyToOrganizeBurrow :: Burrow -> Int
leastEnergyToOrganizeBurrow = accumulatedCost . head . filter (Burrow.isOrganized . node) . aStar step

readInput :: String -> IO Burrow
readInput input = do
  fileName <- getDataFileName ("input-" ++ input)
  content <- readFile fileName
  return . Burrow.fromString $ content

part1 :: IO ()
part1 = do
  input <- readInput "part1"
  putStrLn "Part1: "
  print $ leastEnergyToOrganizeBurrow input

part2 :: IO ()
part2 = do
  input <- readInput "part2"
  putStr "Part2: "
  print $ leastEnergyToOrganizeBurrow input

main :: IO ()
main = do
  part1 -- prints 'Part1: 13556'
  part2 -- prints 'Part2: 54200'

