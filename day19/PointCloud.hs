module PointCloud
  ( PointCloud
  , differenceMap
  , empty
  , fromList
  , intersection
  , map
  , size
  , toList
  , union
  ) where

import Prelude hiding (map)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Point (Point (..))
import qualified Point


newtype PointCloud = PointCloud
  { points :: IntSet
  } deriving (Show, Eq, Ord)

instance Semigroup PointCloud where
  (<>) = union

instance Monoid PointCloud where
  mempty = empty

empty :: PointCloud
empty = fromList []

fromList :: [Point Int] -> PointCloud
fromList = PointCloud . IntSet.fromList . fmap fromEnum

toList :: PointCloud -> [Point Int]
toList = fmap toEnum . IntSet.toList . points

size :: PointCloud -> Int
size = IntSet.size . points

intersection :: PointCloud -> PointCloud -> PointCloud
intersection (PointCloud xs) (PointCloud ys) = PointCloud $ IntSet.intersection xs ys

union :: PointCloud -> PointCloud -> PointCloud
union (PointCloud xs) (PointCloud ys) = PointCloud $ IntSet.union xs ys

map :: (Point Int -> Point Int) -> PointCloud -> PointCloud
map f = PointCloud . IntSet.map (fromEnum . f . toEnum) . points

normalizedDifference :: Point Int -> Point Int -> Point Int
normalizedDifference u v = Point x' y' z'
  where
    [x', y', z'] = List.sort [x, y, z]
    Point x y z = abs $ u - v

differenceMap :: PointCloud -> IntMap PointCloud
differenceMap xs = IntMap.fromListWith (<>)
  [ (fromEnum $ x `normalizedDifference` y, fromList [x, y])
  | (x, y) <- pairs
  ]
  where
    points = toList xs
    pairs = concat . zipWith (fmap . (,)) points . tail . List.tails $ points

