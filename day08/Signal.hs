module Signal
  ( Signal (..)
  , parse
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP


data Signal = A | B | C | D | E | F | G
  deriving (Eq, Ord, Enum)

instance Show Signal where
  show A = "a"
  show B = "b"
  show C = "c"
  show D = "d"
  show E = "e"
  show F = "f"
  show G = "g"

parse :: ReadP Signal
parse = ReadP.choice
  [ ReadP.string "a" >> return A
  , ReadP.string "b" >> return B
  , ReadP.string "c" >> return C
  , ReadP.string "d" >> return D
  , ReadP.string "e" >> return E
  , ReadP.string "f" >> return F
  , ReadP.string "g" >> return G
  ]

instance Read Signal where
  readsPrec _ = ReadP.readP_to_S parse

