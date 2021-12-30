{-# LANGUAGE NamedFieldPuns #-}
module Node
  ( Node (Start, End, Big, Small)
  , parse
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import Data.Functor ((<&>))


data Node
  = Big
    { name :: String
    }
  | Small
    { name :: String
    }
  | Start
  | End
  deriving (Eq, Ord)

instance Show Node where
  show Start = "start"
  show End   = "end"
  show node  = name node

parse :: ReadP Node
parse = foldl1 (ReadP.<++)
  [ ReadP.string "start" >> return Start
  , ReadP.string "end" >> return End
  , ReadP.many1 (ReadP.satisfy Char.isAsciiLower) <&> Small
  , ReadP.many1 (ReadP.satisfy Char.isAsciiUpper) <&> Big
  ]

