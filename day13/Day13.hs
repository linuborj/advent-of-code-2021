{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day13
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Sheet (Sheet)
import qualified Sheet
import Instruction (Instruction)
import qualified Instruction


data Input = Input
  { sheet :: Sheet
  , instructions :: [Instruction]
  } deriving Show

parse :: ReadP Input
parse = do
  sheet <- Sheet.parse
  ReadP.string "\n\n"
  instructions <- Instruction.parse `ReadP.sepBy1` ReadP.char '\n'
  return $ Input
    { sheet = sheet
    , instructions = instructions
    }

parser :: String -> Input
parser = fst . last .ReadP.readP_to_S parse

readInput :: IO Input
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser $ content

followInstruction :: Sheet -> Instruction -> Sheet
followInstruction sheet (Instruction.VerticalFold x)   = Sheet.verticalFold x sheet
followInstruction sheet (Instruction.HorizontalFold y) = Sheet.horizontalFold y sheet

part1 :: IO ()
part1 = do
  Input { sheet, instructions } <- readInput
  let sheet' = followInstruction sheet (head instructions)
  putStr "Part1: "
  print . length . Sheet.dots $ sheet'

part2 :: IO ()
part2 = do
  Input { sheet, instructions } <- readInput
  let sheet' = foldl followInstruction sheet instructions
  putStrLn "Part2: "
  print sheet'

main :: IO ()
main = do
  part1 -- prints 'Part1: 638'
  part2 {- prints:

             Part2: 
              **    **  **  *  * ***   **  ***  *** 
             *  *    * *  * * *  *  * *  * *  * *  *
             *       * *    **   ***  *  * *  * *** 
             *       * *    * *  *  * **** ***  *  *
             *  * *  * *  * * *  *  * *  * *    *  *
              **   **   **  *  * ***  *  * *    *** 
           
           which reads 'CJCKBAPB'
        -}
