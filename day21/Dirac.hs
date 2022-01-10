module Dirac
  ( wins
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Data.Tuple as Tuple
import Player (Player)
import qualified Player


wins :: Player -> Player -> (Int, Int)
wins first second = compute (Player.space first, 0) (Player.space second, 0) `State.evalState` Map.empty

wrap :: Int -> Int
wrap n = 1 + ((n - 1) `mod` 10)

compute :: (Int, Int) -> (Int, Int) -> State (Map ((Int, Int), (Int, Int)) (Int, Int)) (Int, Int)
compute (position, score) (position', score')
  | score' >= 21 = return (0, 1)
  | otherwise = do
    cached <- State.gets (Map.!? ((position, score), (position', score')))
    case cached of
      Just result -> return result
      Nothing -> do
        scenarios <- State.sequence
          [ compute (position', score') (position'', score'')
          | firstRoll <- [1..3]
          , secondRoll <- [1..3]
          , thirdRoll <- [1..3]
          , let rollTotal = firstRoll + secondRoll + thirdRoll
          , let position'' = wrap $ position + rollTotal
          , let score'' = score + position''
          ]
        let wins = Tuple.swap $ foldl1 (\(a, b) (x, y) -> (a + x, b + y)) scenarios
        State.modify $ Map.insert ((position, score), (position', score')) wins
        return wins

