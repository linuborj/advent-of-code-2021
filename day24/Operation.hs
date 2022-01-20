{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Operation
  ( operations
  ) where

import Register (Register (..))
import qualified Expression
import Expression (Expression(..))
import qualified Instruction
import Instruction (Instruction (..))
import qualified Data.List as List


data State = State
  { xExpression :: Expression
  , yExpression :: Expression
  , zExpression :: Expression
  , wExpression :: Expression
  } deriving (Show, Eq, Ord)

operationState :: Int -> State
operationState version = State
  { xExpression = Reference X version
  , yExpression = Reference Y version
  , zExpression = Reference Z version
  , wExpression = Reference W version
  }

(!) ::  State -> Register -> Expression
(!) state@State { xExpression, yExpression, zExpression, wExpression } register = case register of
  X -> xExpression
  Y -> yExpression
  Z -> zExpression
  W -> wExpression

modify :: Register -> (Expression -> Expression) -> State -> State
modify register f state@State { xExpression, yExpression, zExpression, wExpression } = case register of
  X -> state { xExpression = f xExpression }
  Y -> state { yExpression = f yExpression }
  Z -> state { zExpression = f zExpression }
  W -> state { wExpression = f wExpression }

operation :: Int -> [Instruction] -> Expression
operation version instructions = case instructions of
  (Inp register):instructions' -> Expression.reduce . zExpression . foldl apply (operationState version) $ Inp register:instructions'
  _ -> error "first instruction is not input"
  where
    apply state instruction = case instruction of
      Inp register -> modify register (const $ Input register (version+1)) state
      Mul register x -> modify register (:* value x) state
      Add register x -> modify register (:+ value x) state
      Mod register x -> modify register (:% value x) state
      Div register x -> modify register (:/ value x) state
      Eql register x -> modify register (:= value x) state
      where
        value = \case
          Instruction.Number n -> Number n
          Instruction.Reference register -> state ! register

operations :: [Instruction] -> [Expression]
operations = zipWith operation [0..] . chunks
  where
    chunks [] = []
    chunks (op:ops)
      | Instruction.isInput op = (op : takeWhile (not . Instruction.isInput) ops) : chunks (dropWhile (not . Instruction.isInput) ops)
      | otherwise = error "operation does not start with an input"

