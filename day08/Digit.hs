{-# LANGUAGE LambdaCase #-}
module Digit where

import Signal (Signal)
import Data.Set (Set)
import qualified Data.Set as Set

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord, Enum, Show)

pattern :: Digit -> Set Signal
pattern = Set.fromList . map (read . (:[])) . \case
  Zero  -> "abcefg"
  One   -> "cf"
  Two   -> "acdeg"
  Three -> "acdfg"
  Four  -> "bcdf"
  Five  -> "abdfg"
  Six   -> "abdefg"
  Seven -> "acf"
  Eight -> "abcdefg"
  Nine  -> "abcdfg"

integer :: Digit -> Integer
integer = \case
  Zero  -> 0
  One   -> 1
  Two   -> 2
  Three -> 3
  Four  -> 4
  Five  -> 5
  Six   -> 6
  Seven -> 7
  Eight -> 8
  Nine  -> 9

