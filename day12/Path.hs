module Path
  ( Path
  , start
  , to
  ) where

import Node (Node)
import qualified Node
import qualified Data.List as List


newtype Path = Path [Node]
  deriving (Eq, Ord)

instance Show Path where
  show (Path nodes) = List.intercalate "," . map show . reverse $ nodes

start :: Path
start = Path [Node.Start]

to :: Path -> Node -> Path
to (Path nodes) = Path . (: nodes)

