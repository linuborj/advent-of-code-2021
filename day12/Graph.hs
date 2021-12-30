module Graph
  ( Graph
  , (!)
  , nodes
  , parse
  ) where

import Node (Node)
import qualified Node
import Edge (Edge)
import qualified Edge
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Maybe as Maybe
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

newtype Graph = Graph (Map Node [Edge])
  deriving (Show, Eq, Ord)

(!) :: Graph -> Node -> [Edge]
(!) (Graph graph) = Maybe.fromMaybe [] . (graph Map.!?)

nodes :: Graph -> Set Node
nodes (Graph graph) = Map.keysSet graph

parse :: ReadP Graph
parse = do
  edges <- Edge.parse `ReadP.sepBy1` ReadP.char '\n'
  return . Graph $ foldl insert Map.empty edges
  where
    insert graph edge = foldr (\e -> Map.insertWith (++) (Edge.from e) [e]) graph
      [ edge
      , Edge.reverse edge
      ]

