{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Graph
  ( Graph (..)
  , connectedGrid
  )where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


data Graph node cost = Graph
  { nodes :: Set node
  , cost :: node -> cost
  , adjacent :: node -> Set node
  }

instance (Show node, Show cost) => Show (Graph node cost) where
  show Graph { nodes, cost, adjacent } = concat
    [ "Graph { nodes="
    , show nodes
    , ", costs="
    , show costs
    , ", adjacents="
    , show adjacents
    , " }"
    ]
    where
      costs = Map.fromSet cost nodes
      adjacents = Map.fromSet adjacent nodes

connectedGrid :: Num n => [[n]] -> Graph (Integer, Integer) n
connectedGrid xs = Graph
  { nodes    = Map.keysSet grid
  , cost     = (grid Map.!)
  , adjacent = adjacent
  }
  where
    grid = Map.fromList . concat . zipWith (\y -> zipWith ((,) . (, y)) [0..]) [0..] $ xs
    adjacent (x, y) = Set.fromList . filter (`Map.member` grid) $
      [ (x-1, y)
      , (x+1, y)
      , (x, y-1)
      , (x, y+1)
      ]

