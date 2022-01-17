{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Burrow
  ( Burrow
  , fromString
  , isOrganized
  , step
  , minimumEnergyToOrganizeBurrow
  ) where

import Data.Chart (Chart)
import qualified Data.Chart as Chart
import Data.Hashable (Hashable, hashWithSalt)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import qualified Data.Graph.Search as Search
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2
import qualified Data.Maybe as Maybe
import qualified Data.List as List


data Amphipod = Amber | Bronze | Copper | Desert
  deriving (Show, Eq, Ord, Generic, Enum)

instance Hashable Amphipod

data Cell = Unoccupied | Occupied Amphipod
  deriving (Show, Eq, Ord, Generic)

instance Hashable Cell

data Burrow = Burrow 
  { chart :: Chart Cell
  , sideRoom :: Amphipod -> HashSet (V2 Int)
  , hallway :: HashSet (V2 Int)
  }

instance Eq Burrow where
  x == y = chart x == chart y

instance Ord Burrow where
  x `compare` y = chart x `compare` chart y

instance Hashable Burrow where
  hashWithSalt salt burrow = hashWithSalt salt (chart burrow)

instance Show Burrow where
  show = Chart.render (maybe ' ' f) . chart
    where
      f Unoccupied = '.'
      f (Occupied amphipod) = case amphipod of
        Amber -> 'A'
        Bronze -> 'B'
        Copper -> 'C'
        Desert -> 'D'

fromString :: String -> Burrow
fromString input = Burrow
  { chart = chart
  , sideRoom = (sideRooms !!) . fromEnum
  , hallway = hallway
  }
  where
    chart = Chart.mapMaybe cell . Chart.fromString $ input
    cell '.' = Just Unoccupied
    cell 'A' = Just $ Occupied Amber
    cell 'B' = Just $ Occupied Bronze
    cell 'C' = Just $ Occupied Copper
    cell 'D' = Just $ Occupied Desert
    cell _   = Nothing
    (V2 xMin yMin, V2 xMax yMax) = Chart.dimensions chart
    sideRooms =
      [ HashSet.fromList $ map (V2 x) [yMin + 1 .. yMax]
      | x <- [xMin .. xMax]
      , V2 x yMax `Chart.member` chart
      ]
    hallway = HashSet.fromList
      [ V2 x y
      | V2 x y <- Chart.keys chart
      , not $ V2 x yMax `Chart.member` chart
      ]

reachable :: Burrow -> V2 Int -> [V2 Int]
reachable Burrow { chart } = Search.depthFirst neighbors
  where
    neighbors u =
      [ u'
      | du <- [Chart.north, Chart.east, Chart.south, Chart.west]
      , let u' = u + du
      , chart Chart.!? u' == Just Unoccupied
      ]

step :: Burrow -> [(Burrow, Int)]
step burrow@Burrow { chart, sideRoom, hallway } =
  [ (burrow', energySpent)
  | (u, Occupied amphipod) <- Chart.toList chart
  , v <- reachable burrow u
  , isValidPath amphipod u v
  , let burrow' = burrow { chart = Chart.switch u v chart }
  , let energySpent = energy amphipod * distance burrow u v
  ]
  where
    isValidPath amphipod start end
      | start == end = False -- we're not allowed to not move
      | all (`HashSet.member` sideRoom amphipod) [start, end] = False -- we're not allowed to move within the target side-room
      | all (`HashSet.member` hallway) [start, end] = False -- we're not allowed to move within the hallway
      | end `HashSet.member` sideRoom amphipod = isValidSideRoom amphipod end -- we're allowed to move into the target sideroom if it is organized up to a point
      | otherwise = end `HashSet.member` hallway -- we're allowed to move into the hallway
    isValidSideRoom amphipod = all (== Just (Occupied amphipod)) . cellsBelow -- the target side-room is organized if it only contains the correct kind of amphipod and is filled from the back
    cellsBelow = takeWhile (/= Nothing) . map (chart Chart.!?) . tail . iterate (+ Chart.south)

distance Burrow { chart, hallway } u@(V2 x y) v@(V2 i j)
  | inDifferentSideRooms = (y - minY) + (j - minY) + abs (x - i)
  | otherwise = V2.manhattan (u - v)
  where
    inDifferentSideRooms = y > minY && j > minY && x /= i
    (V2 _ minY, _) = Chart.dimensions chart

energy :: Amphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

isOrganized :: Burrow -> Bool
isOrganized Burrow { chart, sideRoom } = and
  [ chart Chart.! u == Occupied amphipod
  | amphipod <- [Amber .. Desert]
  , u <- HashSet.toList $ sideRoom amphipod
  ]

minimumEnergyToOrganizeBurrow :: Burrow -> Int
minimumEnergyToOrganizeBurrow burrow@Burrow { chart, sideRoom } = spentEnergy - normalize
  where
    endOfSideroom = foldl1 (\u v -> max <$> u <*> v) . sideRoom
    normalize = sum
      [ energy amphipod * (n^2 - n) `div` 2
      | amphipod <- [Amber .. Desert]
      , let n = HashSet.size $ sideRoom amphipod
      ]
    spentEnergy = sum
      [ energy amphipod * distance burrow u (endOfSideroom amphipod)
      | (u, Occupied amphipod) <- Chart.toList chart
      ]

