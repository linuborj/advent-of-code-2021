{-# LANGUAGE NamedFieldPuns #-}
module LeastCostTree
  ( LeastCostTree
  , fromGraph
  , leastCost
  , nodes
  , path
  , root
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Graph (Graph)
import qualified Graph
import qualified Data.List as List

data LeastCostTree node cost = LeastCostTree
  { nodes :: Set node
  , root :: node
  , leastCost :: node -> cost
  , adjacent :: node -> Set node
  }

instance (Show node, Show cost) => Show (LeastCostTree node cost) where
  show LeastCostTree { nodes, root, leastCost, adjacent } = concat
    [ "LeastCostTree { nodes="
    , show nodes
    , ", root="
    , show root
    , ", leastCosts="
    , show leastCosts
    , ", adjacents="
    , show adjacents
    , " }"
    ]
    where
      leastCosts = Map.fromSet leastCost nodes
      adjacents = Map.fromSet adjacent nodes

fromGraph :: Graph node cost -> node -> (node -> cost) -> LeastCostTree node cost
fromGraph graph root leastCost = LeastCostTree
  { nodes = Graph.nodes graph
  , root = root
  , leastCost = leastCost
  , adjacent = Graph.adjacent graph
  }

path :: (Eq node, Ord cost) => LeastCostTree node cost -> node -> [node]
path leastCostTree@LeastCostTree { leastCost, adjacent, root } node
  | node == root = [root]
  | otherwise = node : path leastCostTree node'
  where
    node' = head . List.sortOn leastCost . Set.toList . adjacent $ node

