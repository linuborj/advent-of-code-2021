{-# LANGUAGE NamedFieldPuns #-}
module Explore
  ( paths
  , Rule (..)
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
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


data Thread = Thread
  { path       :: Path
  , visited    :: Map Node Int
  , node       :: Node
  , mostVisitsToASmallNode :: Int
  } deriving (Show, Eq, Ord)

data Rule = SmallNodesMayOnlyBeVisitedOnce | ASingleSmallNodeMayBeVisitedTwcice
  deriving Show

start :: Thread
start = Thread
  { path       = Path.start
  , visited    = Map.singleton Node.Start 1
  , node       = Node.Start
  , mostVisitsToASmallNode = 0
  }

forward :: Thread -> Node -> Thread
forward thread@Thread { path, visited, mostVisitsToASmallNode } destination = Thread
  { path    = path `Path.to` destination
  , visited = Map.insertWith (+) destination 1 visited
  , node = destination
  , mostVisitsToASmallNode = mostVisitsToASmallNode `max` destinationVisits
  }
  where
    destinationVisits = case destination of
      Node.Small {} -> 1 + thread `visits` destination
      _             -> mostVisitsToASmallNode

visits :: Thread -> Node -> Int
visits Thread { visited } = Maybe.fromMaybe 0 . (visited Map.!?)

branch :: Rule -> Graph -> Thread -> [Thread]
branch rule graph thread@Thread{ path, visited, node, mostVisitsToASmallNode } = map (thread `forward`) destinations
  where
    destinations = filter isValidDestination . map Edge.to $ graph Graph.! node
    isValidDestination destination = case destination of
      Node.Start    -> False
      Node.End      -> True
      Node.Big {}   -> True
      Node.Small {} -> case rule of
        SmallNodesMayOnlyBeVisitedOnce -> thread `visits` destination < 1
        ASingleSmallNodeMayBeVisitedTwcice -> thread `visits` destination < (if mostVisitsToASmallNode < 2 then 2 else 1)

threads :: Rule -> Graph -> [Thread]
threads rule graph = threads' start
  where
    threads' thread
      | node thread == Node.End = [thread]
      | otherwise = concatMap threads' $ branch rule graph thread

paths :: Rule -> Graph -> Set Path
paths rule = Set.fromList . map path . threads rule

