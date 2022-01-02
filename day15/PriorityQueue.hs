{-# LANGUAGE NamedFieldPuns #-}
module PriorityQueue
  ( PriorityQueue
  , empty
  , null
  , push
  , pop
  , pop'
  ) where

import Prelude hiding (null)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Graph (Graph (..))
import qualified Graph
import qualified Data.Maybe as Maybe


data Entry item priority = Entry
  { item :: item
  , priority :: priority
  } deriving (Eq, Show)

instance (Ord item, Ord priority) => Ord (Entry item priority) where
  x `compare` y = case priority x `compare` priority y of
    EQ -> item x `compare` item y
    comparison -> comparison

data PriorityQueue item cost = PriorityQueue
  { costs :: Map item cost
  , queue :: Set (Entry item cost)
  }

instance (Show item, Show cost) => Show (PriorityQueue item cost) where
  show PriorityQueue { queue } = "PriorityQueue { entries = " ++ show entries ++ " }"
    where
      entries = map (\Entry { item, priority } -> (item, priority)) . Set.toList $ queue

empty :: PriorityQueue item cost
empty = PriorityQueue Map.empty Set.empty

push :: (Ord item, Ord cost) => (item, cost) -> PriorityQueue item cost -> PriorityQueue item cost
push (item, cost) priorityQueue@PriorityQueue { costs, queue } = priorityQueue
  { costs = Map.insertWith min item cost costs
  , queue = case costs Map.!? item of
      Nothing    -> Entry item cost `Set.insert` queue
      Just cost' -> Entry item (cost `min` cost') `Set.insert` (Entry item cost' `Set.delete` queue)
  }

pop :: Ord item => PriorityQueue item cost -> Maybe ((item, cost), PriorityQueue item cost)
pop priorityQueue@PriorityQueue { costs, queue } = transform <$> Set.minView queue
  where
    transform (Entry item cost, queue') = ((item, cost), priorityQueue
      { costs = item `Map.delete` costs
      , queue = queue'
      })

pop' :: Ord item => PriorityQueue item cost -> ((item, cost), PriorityQueue item cost)
pop' = Maybe.fromJust . pop

null :: PriorityQueue item cost -> Bool
null = Set.null . queue

