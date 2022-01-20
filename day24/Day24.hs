module Main where

import Paths_day24
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import Instruction (Instruction)
import qualified Instruction
import qualified Expression
import qualified Operation
import Text.Subscript (subscript)
import Register (Register (..))
import Expression (Expression (..))
import qualified Control.Monad as Monad

inputParser :: ReadP [Instruction]
inputParser = (Instruction.parser `ReadP.sepBy` ReadP.char '\n') <* ReadP.munch (== '\n')

readInput :: IO [Expression]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Operation.operations . Maybe.fromJust . ReadP.unambiguosParser inputParser $ content

printOperations :: IO ()
printOperations = do
  operations <- readInput
  mapM_ printOperation $ zip [1..] operations
  where
    printOperation (i, operation) = putStrLn $ "z" ++ subscript i ++ ":=" ++ show operation

data ActionStack
  = LeftShift Int Int
  | RightShift Int Int
  deriving (Show, Eq, Ord)

-- Hackish to say the least :)
fromExpression :: Expression -> ActionStack
fromExpression ((Reference Z _ :* ((_ :* (((_ :+ Number r) := _) := _)) :+ _)) :+ ((_ :+ Number s) :* _)) = LeftShift r s
fromExpression ((Reference Z _ :* ((_ :* (((_ :+ Number r) := _) := _)) :+ _)) :+ (_ :* _)) = LeftShift r 0
fromExpression ((((Reference Z _ :/ _)) :* ((_ :* (((_ :+ Number r) := _) := _)) :+ _)) :+ ((_ :+ Number s) :* _)) = RightShift r s
fromExpression _ = error "expression is neither a left shift nor a right shift"

solve :: [Expression] -> [[Int]]
solve = solve 0 . map fromExpression
  where
    solve 0 [] = [[]]
    solve _ [] = []
    solve z (action:actions) = case action of
      LeftShift r s -> do
        w <- [1..9]
        let u = if ((z `mod` 26) + r) /= w then 1 else 0
        Monad.guard $ u == 1
        let z' = (z * 26) + w + s
        map (w:) $ solve z' actions
      RightShift r s -> do
        w <- [1..9]
        let u = if ((z `mod` 26) + r) /= w then 1 else 0
        Monad.guard $ u == 0
        let z' = z `div` 26
        map (w:) $ solve z' actions

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  putStrLn . concatMap show . last . solve $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  putStrLn . concatMap show . head . solve $ input

main :: IO ()
main = do
  part1 -- prints 'Part1: 29991993698469'
  part2 -- prints 'Part2: 14691271141118'

