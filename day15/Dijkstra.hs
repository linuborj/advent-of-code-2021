{-# LANGUAGE NamedFieldPuns #-}
module Dijkstra where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Graph (Graph (..))
import qualified Graph
import IntPQueue (IntPQueue)
import qualified IntPQueue
import LeastCostTree (LeastCostTree)
import qualified LeastCostTree


data Dijkstra node = Dijkstra
  { candidates :: IntPQueue node
  , leastCosts :: IntMap Int
  , graph :: Graph node Int
  }

initialize :: Graph node Int -> node -> Dijkstra node
initialize graph root = Dijkstra
  { candidates = (0, root) `IntPQueue.insert` IntPQueue.empty  -- We only count the cost to enter a square. I.e. not the cost of the first square
  , leastCosts = IntMap.empty
  , graph = graph
  }

step :: Enum node => Dijkstra node -> Dijkstra node
step dijkstra@Dijkstra { candidates, leastCosts, graph }
  -- We've already encountered this node before. We do not have to add it's adjacent nodes again. All we have to do is
  --   - update the leastCost for this node if it is lower, and
  --   - remove the node from the least of candidates
  | fromEnum node `IntMap.member` leastCosts = dijkstra  
    { leastCosts = IntMap.insertWith min (fromEnum node) leastCost leastCosts
    , candidates = candidates'
    }
  -- This is a node that we have not encountered before, we have to
  --   - update the leastCost of the node, ad
  --   - add its adjacent nodes to the list of candidates.
  | otherwise = dijkstra
    { candidates = foldr addCandidate candidates' adjacents
    , leastCosts = IntMap.insert (fromEnum node) leastCost leastCosts
    }
  where
    ((leastCost, node), candidates') = Maybe.fromJust $ IntPQueue.minView candidates -- fromJust is hackish :)
    addCandidate node = IntPQueue.insert (leastCost + graph `Graph.cost` node, node)
    adjacents = Set.filter (not . (`IntMap.member` leastCosts) . fromEnum) $ graph `Graph.adjacent` node

leastCostTree :: Enum node => Graph node Int -> node -> LeastCostTree node Int
leastCostTree graph root = LeastCostTree.fromGraph graph root ((leastCosts IntMap.!) . fromEnum)
  where
    Dijkstra { leastCosts } = head . filter (IntPQueue.null . candidates) . iterate step $ initialize graph root

