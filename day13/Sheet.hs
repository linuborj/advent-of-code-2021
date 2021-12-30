{-# LANGUAGE NamedFieldPuns #-}
module Sheet
  ( Sheet
  , dots
  , horizontalFold
  , parse
  , verticalFold
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char


newtype Sheet = Sheet (Set (Int, Int))

data Dimensions = Dimensions
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  }

dimensions :: Sheet -> Dimensions
dimensions (Sheet dots) = Dimensions
  { minX
  , maxX
  , minY
  , maxY
  }
  where
    [minX, maxX] = map ($ Set.map fst dots) [minimum, maximum]
    [minY, maxY] = map ($ Set.map snd dots) [minimum, maximum]

instance Show Sheet where
  show sheet = rows
    where
      Dimensions { minX, maxX, minY, maxY } = dimensions sheet
      character x y
        | (x, y) `member` sheet = '*'
        | otherwise             = ' '
      rows = unlines
        [ map (`character` y) [minX .. maxX]
        | y <- [minY .. maxY]
        ]

member :: (Int, Int) -> Sheet -> Bool
member (x, y) (Sheet dots ) = (x, y) `Set.member` dots

transform :: ((Int, Int) -> (Int, Int)) -> Sheet -> Sheet
transform f (Sheet dots) = Sheet $ Set.map f dots

verticalFold :: Int -> Sheet -> Sheet
verticalFold x = transform fold
  where
    fold (i, j)
      | i < x     = (i, j)
      | otherwise = (2 * x - i, j)
      
horizontalFold :: Int -> Sheet -> Sheet
horizontalFold y = transform fold
  where
    fold (i, j)
      | j < y     = (i, j)
      | otherwise = (i, 2 * y - j)

dots :: Sheet -> Set (Int, Int)
dots (Sheet dots) = dots

parseCoodinate :: ReadP (Int, Int)
parseCoodinate = do
  x <- ReadP.many1 $ ReadP.satisfy Char.isDigit
  ReadP.string ","
  y <- ReadP.many1 $ ReadP.satisfy Char.isDigit
  return (read x, read y)

parse :: ReadP Sheet
parse = do
  coordinates <- parseCoodinate `ReadP.sepBy1` ReadP.char '\n'
  return . Sheet . Set.fromList $ coordinates

