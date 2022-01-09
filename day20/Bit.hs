module Bit
  ( Bit (..)
  , int
  , parser
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

data Bit
  = Zero
  | One
  deriving (Eq, Ord)

instance Show Bit where
  show Zero = "."
  show One = "#"

int :: [Bit] -> Int
int = sum . zipWith (*) (map (2^) [0..]) . map (\bit -> if bit == Zero then 0 else 1) . reverse 

parser :: ReadP Bit
parser = foldl1 (ReadP.<++)
  [ ReadP.char '.' >> return Zero
  , ReadP.char '#' >> return One
  ]

