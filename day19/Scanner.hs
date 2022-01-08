{-# LANGUAGE NamedFieldPuns #-}
module Scanner
  ( Scanner (..)
  , differenceMap
  , map
  , parser
  ) where

import Prelude hiding (map)
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified PointCloud
import PointCloud (PointCloud)
import Point (Point (..))
import Data.IntMap (IntMap)


data Scanner = Scanner
  { identifier :: Int
  , beacons :: PointCloud
  } deriving (Show, Eq, Ord)

map :: (Point Int -> Point Int) -> Scanner -> Scanner
map f scanner@Scanner { beacons } = scanner
  { beacons = PointCloud.map f beacons
  }

differenceMap :: Scanner -> IntMap PointCloud
differenceMap = PointCloud.differenceMap . beacons

asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

intParser :: ReadP Int
intParser = foldl1 (ReadP.<++)
  [ ReadP.char '-' *> (negate <$> positive)
  , positive
  ]
  where
    positive = do
      digits <- asManyAsPossible . ReadP.satisfy $ Char.isDigit
      Monad.guard $ digits /= ""
      return $ read digits

coordinateParser :: ReadP (Point Int)
coordinateParser = Point <$> intParser <* ReadP.char ',' <*> intParser <* ReadP.char ',' <*> intParser

separatedBy :: ReadP a -> Char -> ReadP [a]
separatedBy parser c = asManyAsPossible $ foldl1 (ReadP.<++)
  [ parser <* ReadP.char c
  , parser
  ]

pointCloudParser :: ReadP PointCloud
pointCloudParser = PointCloud.fromList <$> coordinateParser `separatedBy` '\n'

parser :: ReadP Scanner
parser = do
  ReadP.string "--- scanner "
  number <- intParser
  ReadP.string " ---\n"
  Scanner number <$> pointCloudParser

