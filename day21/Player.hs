module Player 
  ( Player (..)
  , parser
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe


data Player = Player
  { identifier:: Int
  , space :: Int
  , score :: Int
  } deriving (Show, Eq, Ord)

asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

parser :: ReadP Player
parser =  do
  ReadP.string "Player "
  identifier <- read <$> asManyAsPossible (ReadP.satisfy Char.isDigit)
  ReadP.string " starting position: "
  space <- read <$> asManyAsPossible (ReadP.satisfy Char.isDigit)
  ReadP.char '\n'
  return $ Player
    { identifier = identifier
    , space = space
    , score = 0
    }

