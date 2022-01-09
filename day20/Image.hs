{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Image
  ( Image
  , pixels
  , fromList
  , evolve
  ) where

import Prelude hiding ((!!))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Bit (Bit (..))
import qualified Bit
import qualified Data.Bifunctor as Bifunctor


data Image = Image
  { pixels :: IntMap Bit
  , dimensions :: ((Int, Int), (Int, Int))
  , outside :: Bit
  } deriving (Show, Eq)

encode :: (Int, Int) -> Int
encode (x, y) = 1_000_000 * (x + 500_000) + y + 500_000

decode :: Int -> (Int, Int)
decode xy = (x - 500_000, y - 500_000)
  where
    (x, y) = xy `divMod` 1_000_000

fromList :: [((Int, Int), Bit)] -> Image
fromList pixels = Image
  { pixels = IntMap.fromList . map (Bifunctor.first encode) $ pixels
  , dimensions = foldl dimension (c, c) cs
  , outside = Zero
  }
  where
    c:cs = map fst pixels
    dimension ((xMin, xMax), (yMin, yMax)) (x, y) = ((x `min` xMin, x `max` xMax), (y `min` yMin, y `max` yMax))

expand :: Image -> Image
expand image@Image { dimensions = ((xMin, xMax), (yMin, yMax)) } = image
  { dimensions = ((xMin - 1, xMax + 1), (yMin - 1, yMax + 1))
  }

updatePixels :: IntMap Bit -> Image -> Image
updatePixels mapping image@Image { pixels, dimensions = ((xMin, xMax), (yMin, yMax)), outside } = image
  { pixels = foldl update IntMap.empty [ encode (x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax] ]
  , outside = mapping IntMap.! Bit.int (replicate 9 outside)
  }
  where
    lookup (x, y) = IntMap.findWithDefault outside (encode (x, y)) pixels
    update i k = IntMap.insert k (mapping IntMap.! value k) i
    value k = Bit.int
      [ lookup (x', y') 
      | let (x, y) = decode k
      , y' <- [y-1, y, y+1]
      , x' <- [x-1, x, x+1]
      ]

evolve :: IntMap Bit -> Image -> Image
evolve mapping = updatePixels mapping . expand

