{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day20
import Bit (Bit (..))
import qualified Bit
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Image (Image)
import qualified Image
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

inputParser :: ReadP (IntMap Bit, Image)
inputParser = (,) <$> mappingParser <*> imageParser
  where 
    bitsParser = asManyAsPossible Bit.parser
    index = concat . zipWith (\y -> zipWith ((,) . (, y)) [0..]) [0..]
    imageParser = Image.fromList .index <$> asManyAsPossible (ReadP.char '\n' *> bitsParser)
    mappingParser = IntMap.fromList . zip [0..] <$> bitsParser <* ReadP.char '\n'

parser :: ReadP a -> String -> a
parser p s = case ReadP.readP_to_S p s of
  [(parsed, "")] -> parsed
  _ -> error "could not parse"

readInput :: IO (IntMap Bit, Image)
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser inputParser $ content

part1 :: IO ()
part1 = do
  (mapping, image) <- readInput
  let images = iterate (Image.evolve mapping) image
  putStr "Part1: "
  print . IntMap.size . IntMap.filter (==One) . Image.pixels $ images !! 2

part2 :: IO ()
part2 = do
  (mapping, image) <- readInput
  let images = iterate (Image.evolve mapping) image
  putStr "Part2: "
  print . IntMap.size . IntMap.filter (==One) . Image.pixels $ images !! 50

main :: IO ()
main = do
  part1 -- prints 'Part1: 5836'
  part2 -- prints 'Part2: 21149'

