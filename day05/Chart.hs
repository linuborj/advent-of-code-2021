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

newtype Chart = Chart { points :: Map Coordinate Int }
  deriving (Show, Eq)

emptyChart :: Chart 
emptyChart = Chart Map.empty

pointsWhere :: Chart -> (Int -> Bool) -> [(Coordinate, Int)]
pointsWhere (Chart points) = Map.toList . (`Map.filter` points)

isHorizontal :: Line -> Bool
isHorizontal ((_, y), (_, v)) = y == v

isVertical :: Line -> Bool
isVertical ((x, _), (u, _)) = x == u

registerLine :: Chart -> Line -> Chart
registerLine (Chart points) line@((x, y), (u, v))
  | isHorizontal line = Chart $ register horizontals
  | isVertical line = Chart $ register verticals
  | otherwise = error "Can only register verical and horizontal lines"
  where
    register xs = foldr (\c -> Map.insertWith (+) c 1) points xs
    horizontals = [ (x', y) | x' <- [min x u..max x u] ]
    verticals   = [ (x, y') | y' <- [min y v..max y v] ]

