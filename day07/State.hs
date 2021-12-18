{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module State
  ( Position
  , Cost
  , Count
  , State
  , state
  , count
  , range
  , CostFunction
  , costFunction
  ) where

import qualified Data.Map as Map
import Data.Map (Map)


newtype Position = Position { unPosition :: Integer }
  deriving (Num, Eq, Ord, Enum)

instance Show Position where
  show = show . unPosition

newtype Cost = Cost { unCost :: Integer }
  deriving (Num, Eq, Ord, Enum)

instance Show Cost where
  show = show . unCost

newtype Count = Count { unCount :: Integer }
  deriving (Num, Eq, Ord, Enum)

instance Show Count where
  show = show . unCount

newtype State = State { chart :: Map Position Count }
  deriving (Show, Eq)

state :: Integral a => [a] -> State
state = State . foldr insert Map.empty
  where
    insert = flip (Map.insertWith (+)) 1 . fromIntegral

count :: State -> Position -> Count
count State { chart } position = Map.findWithDefault 0 position chart

range :: State -> [Position]
range State { chart } = [minPosition..maxPosition]
  where
    minPosition  = fst $ Map.findMin chart
    maxPosition  = fst $ Map.findMax chart

type CostFunction = Count -> Cost -> Cost

costFunction :: (Integer -> Integer -> Integer) -> CostFunction
costFunction f (Count count) (Cost cost) = Cost $ f count cost

