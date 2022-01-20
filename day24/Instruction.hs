module Instruction
  ( Register (..)
  , Value (..)
  , Instruction (..)
  , isInput
  , parser
  ) where

import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import Register (Register (..))
import qualified Register


data Value
  = Reference Register
  | Number Int
  deriving (Show, Eq, Ord)

valueParser :: ReadP Value
valueParser = foldl1 (ReadP.<++)
  [ Reference <$> Register.parser
  , Number <$> ReadP.int
  ]

data Instruction
  = Inp Register
  | Mul Register Value
  | Add Register Value 
  | Mod Register Value 
  | Div Register Value 
  | Eql Register Value 
  deriving (Show, Eq, Ord)

isInput :: Instruction -> Bool
isInput (Inp _) = True
isInput _ = False

parser :: ReadP Instruction
parser = foldl1 (ReadP.<++)
  [ unary Inp "inp "
  , binary Mul "mul "
  , binary Add "add "
  , binary Mod "mod "
  , binary Div "div "
  , binary Eql "eql "
  ]
  where
    unary constructor prefix = constructor <$> (ReadP.string prefix *> Register.parser)
    binary constructor prefix  = constructor <$> (ReadP.string prefix *> Register.parser) <*> (ReadP.char ' ' *> valueParser)

