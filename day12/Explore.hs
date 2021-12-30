{-# LANGUAGE NamedFieldPuns #-}
module Explore
  ( paths
  ) where

import Graph (Graph)
import qualified Graph
import Path (Path)
import qualified Path
import Node (Node)
import qualified Node
import Edge (Edge)
import qualified Edge
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Control.Monad as Monad


data Thread = Thread
  { path    :: Path
  , visited :: Set Node
  , node    :: Node
  } deriving (Show, Eq, Ord)

start :: Thread
start = Thread
  { path    = Path.start
  , visited = Set.singleton Node.Start
  , node    = Node.Start
  }

forward :: Thread -> Node -> Thread
forward Thread { path, visited } destination = Thread
  { path    = path `Path.to` destination
  , visited = destination `Set.insert` visited
  , node = destination
  }

branch :: Graph -> Thread -> [Thread]
branch graph thread@Thread{ path, visited, node } = map (thread `forward`) destinations
  where
    destinations = filter isValidDestination . map Edge.to $ graph Graph.! node
    isValidDestination destination = case destination of
      Node.Start    -> False
      Node.End      -> True
      Node.Big {}   -> True
      Node.Small {} -> not $ destination `Set.member` visited

threads :: Graph -> [Thread]
threads graph = threads' start
  where
    threads' thread
      | node thread == Node.End = [thread]
      | otherwise = concatMap threads' $ branch graph thread

paths :: Graph -> Set Path
paths = Set.fromList . map path . threads

