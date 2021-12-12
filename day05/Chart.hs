module Chart
  ( Coordinate
  , Line
  , Chart (..)
  , emptyChart
  , pointsWhere
  , isHorizontal
  , isVertical
  , registerLine
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

type Line = (Coordinate, Coordinate)

newtype Chart = Chart { heightMap :: Map Coordinate Int }
  deriving (Show, Eq)

emptyChart :: Chart
emptyChart = Chart Map.empty

pointsWhere :: Chart -> (Int -> Bool) -> [(Coordinate, Int)]
pointsWhere (Chart heightMap) = Map.toList . (`Map.filter` heightMap)

isHorizontal :: Line -> Bool
isHorizontal ((_, y), (_, v)) = y == v

isVertical :: Line -> Bool
isVertical ((x, _), (u, _)) = x == u

isDiagonal :: Line -> Bool
isDiagonal ((x, y), (u, v)) = abs (x - u) == abs (y - v)

points :: Line -> [Coordinate]
points ((x, y), (u, v)) = zip [x, x + dx .. u] [y, y + dy .. v]
  where
    dx = signum $ u - x
    dy = signum $ v - y

registerLine :: Chart -> Line -> Chart
registerLine (Chart heightMap) line
  | isValid   = Chart . register $ points line
  | otherwise = error "Can only register verical, horizontal, and diagonal lines"
  where
    isValid = any ($ line) [isHorizontal, isVertical, isDiagonal]
    register xs = foldr (\c -> Map.insertWith (+) c 1) heightMap xs
