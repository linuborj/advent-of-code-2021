module Instruction
  ( Instruction (..)
  , parse
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char

data Instruction
  = HorizontalFold Int
  | VerticalFold Int
  deriving (Show, Read, Eq)

parseCoodinate :: ReadP (Int, Int)
parseCoodinate = do
  x <- ReadP.many1 $ ReadP.satisfy Char.isDigit
  ReadP.string ","
  y <- ReadP.many1 $ ReadP.satisfy Char.isDigit
  return (read x, read y)

parse :: ReadP Instruction
parse = do
  ReadP.string "fold along "
  instruction <- foldl1 (ReadP.<++)
    [ ReadP.char 'x' >> return VerticalFold
    , ReadP.char 'y' >> return HorizontalFold
    ]
  ReadP.string "="
  number <- ReadP.many1 $ ReadP.satisfy Char.isDigit
  return . instruction . read $ number

