{-# LANGUAGE NamedFieldPuns #-}
module Edge
  ( Edge
  , from
  , to
  , reverse
  , parse
  ) where

import Prelude hiding (reverse)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Node (Node)
import qualified Node


data Edge = Edge
  { from :: Node
  , to   :: Node
  } deriving (Eq, Ord)

instance Show Edge where
  show Edge { from, to } = show from ++ "-" ++ show to

reverse :: Edge -> Edge
reverse Edge { from, to } = Edge { from = to, to = from }

parse :: ReadP Edge
parse = do
  from <- Node.parse
  ReadP.string "-"
  to <- Node.parse
  return $ Edge
    { from
    , to
    }

