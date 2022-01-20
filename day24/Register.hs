module Register
  ( Register (..)
  , parser
  ) where

import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP


data Register = X | Y | Z | W
 deriving (Eq, Ord, Enum)

instance Show Register where
  show X = "x"
  show Y = "y"
  show Z = "z"
  show W = "w"

parser :: ReadP Register
parser = foldl1 (ReadP.<++)
  [ ReadP.string "x" >> return X
  , ReadP.string "y" >> return Y
  , ReadP.string "z" >> return Z
  , ReadP.string "w" >> return W
  ]

