{-# LANGUAGE NamedFieldPuns #-}
module State
  ( State
  , Statistics ( .. )
  , statistics
  , evolve
  , parse
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import qualified Data.Tuple as Tuple


data State = State
  { pairCount :: Map String Integer
  , charCount :: Map Char Integer
  , rules     :: Map String Char
  } deriving Show

data Statistics = Statistics
  { mostCommonElement :: (Char, Integer)
  , leastCommonElement :: (Char, Integer)
  } deriving Show

statistics :: State -> Statistics
statistics State { charCount } = Statistics
  { mostCommonElement  = Tuple.swap . maximum . map Tuple.swap . Map.toList $ charCount
  , leastCommonElement = Tuple.swap . minimum . map Tuple.swap . Map.toList $ charCount
  }

accumulateInto :: (Ord k, Num v) => [(k, v)] -> Map k v -> Map k v
accumulateInto xs acc = foldl (\acc (k, v) -> Map.insertWith (+) k v acc) acc xs

step :: State -> State
step state@State { pairCount, charCount, rules } = state
  { pairCount = (concatMap applyPairRule . Map.toList $ pairCount) `accumulateInto` Map.empty
  , charCount = (concatMap applyCharRule . Map.toList $ pairCount) `accumulateInto` charCount
  }
  where
    applyCharRule (pair, count) = case rules Map.!? pair of
      Nothing -> []
      Just c  -> [(c, count)]
    applyPairRule (pair, count) = case rules Map.!? pair of
      Nothing -> [(pair, count)]
      Just c  -> let [x, y] = pair in [([x, c], count), ([c, y], count)]

evolve :: State -> [State]
evolve = iterate step

chunk :: String -> [String]
chunk xs = zipWith (\x y -> [x, y]) xs (tail xs)

parseRules :: ReadP (Map String Char)
parseRules = do
  rules <- parse' `ReadP.sepBy1` ReadP.char '\n'
  return . Map.fromList $ rules
  where 
    parse' = do
      x <- ReadP.satisfy Char.isAsciiUpper
      y <- ReadP.satisfy Char.isAsciiUpper
      ReadP.string " -> "
      char <- ReadP.satisfy Char.isAsciiUpper
      return ([x, y], char)

parse :: ReadP State
parse = do
  template <- ReadP.many1 $ ReadP.satisfy Char.isAsciiUpper
  ReadP.string "\n\n"
  rules <- parseRules
  return $ State
    { pairCount = foldr (\pair -> Map.insertWith (+) pair 1) Map.empty $ chunk template
    , charCount = foldr (\c -> Map.insertWith (+) c 1) Map.empty template
    , rules     = rules
    }

