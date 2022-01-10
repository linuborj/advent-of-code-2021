{-# LANGUAGE NamedFieldPuns #-}
module Deterministic
  ( Result (..)
  , Roll (..)
  , play
  ) where

import Player (Player (..))
import qualified Data.Maybe as Maybe


data Roll = Roll
  { value :: Int
  , number :: Int
  } deriving (Show, Eq, Ord)

rolls :: [Roll]
rolls = zipWith Roll (triples . concat $ repeat [1..100]) [3,6..]
  where
    triples (x:y:z:xs) = sum [x,y,z]:triples xs

wrap :: Int -> Int
wrap space = 1 + ((space - 1) `mod` 10)

move :: Player -> Roll -> Player
move player@Player { space, score } Roll { value } = player
  { space = space'
  , score = score + space'
  }
  where
    space' = wrap $ space + value

data State
  = InitialState
    { next :: Player
    , previous :: Player
    }
  | State
    { next :: Player
    , previous :: Player
    , roll :: Roll
    }

data Result = Result
  { winner :: Player
  , loser :: Player
  , lastRoll :: Roll
  } deriving (Show, Eq, Ord)

result :: State -> Maybe Result
result InitialState { } = Nothing
result State { next, previous, roll }
  | score previous < 1000 = Nothing
  | otherwise = Just Result
    { winner = previous
    , loser = next
    , lastRoll = roll
    }

play :: (Player, Player) -> Result
play (first, second) = head . Maybe.mapMaybe result $ scanl step (InitialState first second) rolls
  where
    step :: State -> Roll -> State
    step state roll = State
      { next = previous state
      , previous = next state `move` roll
      , roll = roll
      }

