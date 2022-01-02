{-# LANGUAGE NamedFieldPuns #-}
module Dijkstra
  ( leastCostTree
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Graph (Graph (..))
import qualified Graph
import PriorityQueue (PriorityQueue)
import qualified PriorityQueue
import LeastCostTree (LeastCostTree)
import qualified LeastCostTree


data Dijkstra node cost = Dijkstra
  { candidates :: PriorityQueue node cost
  , leastCosts :: Map node cost
  , graph :: Graph node cost
  } deriving Show

initialize :: (Ord node, Ord cost, Num cost) => Graph node cost -> node -> Dijkstra node cost
initialize graph root = Dijkstra
  {candidates = (root, 0) `PriorityQueue.push` PriorityQueue.empty  -- We only count the cost to enter a square. I.e. not the cost of the first square
  , leastCosts = Map.empty
  , graph = graph
  }

step :: (Ord node, Ord cost, Num cost) => Dijkstra node cost -> Dijkstra node cost
step dijkstra@Dijkstra { candidates, leastCosts, graph } = dijkstra
  { candidates = foldr addCandidate candidates' adjacents
  , leastCosts = Map.insert node leastCost leastCosts
  }
  where
    ((node, leastCost), candidates') = PriorityQueue.pop' candidates
    addCandidate node = PriorityQueue.push (node, leastCost + graph `Graph.cost` node)
    adjacents = graph `Graph.adjacent` node Set.\\ Map.keysSet leastCosts

leastCostTree :: (Ord node, Ord cost, Num cost) => Graph node cost -> node -> LeastCostTree node cost
leastCostTree graph root = LeastCostTree.fromGraph graph root (leastCosts Map.!)
  where
    Dijkstra { leastCosts } = head . filter (PriorityQueue.null . candidates) . iterate step $ initialize graph root

