{-# LANGUAGE NamedFieldPuns #-}
module Cost
  ( fuelCosts
  ) where

import State
import qualified Data.Map as Map
import Data.Map (Map)


data Accumulator = Accumulator
  { position :: Position
  , tally :: Count
  , total :: Cost
  } deriving (Show, Eq)

accumulate :: CostFunction -> Accumulator -> (Position, Count) -> Accumulator
accumulate f Accumulator { tally, total } (position, count) = let tally' = tally + count 
                                                                  total' = total + f tally
                                                              in Accumulator position tally' total'

scanLeft' :: CostFunction -> State -> [Position] -> [Accumulator]
scanLeft' f state positions = scanl (accumulate f) (Accumulator postion0 count0 0) steps
  where
    (postion0, count0):steps = map (\position -> (position, state `count` position)) positions

scanLeft :: CostFunction -> State -> [Accumulator]
scanLeft f state = scanLeft' f state $ range state

scanRight :: CostFunction -> State -> [Accumulator]
scanRight f state = scanLeft' f state . reverse $ range state

fuelCosts :: CostFunction -> State -> Map Position Cost
fuelCosts f state = foldr insert Map.empty $ scanLeft f state ++ scanRight f state
  where
    insert Accumulator { position, total } = Map.insertWith (+) position total

