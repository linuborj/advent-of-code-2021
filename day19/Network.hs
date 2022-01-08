module Network where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Scanner (Scanner)
import qualified Scanner
import PointCloud (PointCloud)
import qualified PointCloud
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Point (Point (..))
import qualified Point
import Data.Set (Set)
import qualified Data.Set as Set


data Network
  = Root Scanner
  | Link Network (Point Int -> Point Int) Scanner

instance Show Network where
  show (Root scanner) = show $ Scanner.identifier scanner
  show (Link network _ scanner) = show (Scanner.identifier scanner) ++ " -> " ++ show network

beacons :: Network -> PointCloud
beacons (Root scanner) = Scanner.beacons scanner
beacons (Link network transformer scanner) = beacons network <> Scanner.beacons (transformer `Scanner.map` scanner)

scannerPositions :: Network -> [Point Int]
scannerPositions (Root scanner) = [Point 0 0 0]
scannerPositions (Link network transform scanner) = [transform $ Point 0 0 0] <> scannerPositions network

differenceMap :: Network -> IntMap PointCloud
differenceMap (Root scanner) = Scanner.differenceMap scanner
differenceMap (Link network transformer scanner) = IntMap.unionWith (<>) (differenceMap network) (Scanner.differenceMap $ transformer `Scanner.map` scanner)

fromList :: [Scanner] -> Maybe Network
fromList [] = Nothing
fromList (root:scanners) = Maybe.listToMaybe $ search (Root root) (Set.fromList scanners)
  where
    search network scanners
      | Set.null scanners = [network]
      | otherwise = do
        scanner <- scanners'
        network' <- network `links` scanner
        search network' $ scanner `Set.delete` scanners
      where
        differencesInCommon scanner = negate . IntMap.size $ differenceMap network `IntMap.intersection` Scanner.differenceMap scanner
        scanners' = List.sortOn differencesInCommon $ Set.toList scanners

links :: Network -> Scanner -> [Network]
links network scanner = do
  transform <- network `alignments` scanner
  return $ Link network transform scanner

alignments :: Network -> Scanner -> [Point Int -> Point Int]
alignments network scanner =
  [ transform
  | (xs, ys) <- candidates
  , x <- PointCloud.toList xs
  , y <- PointCloud.toList ys
  , rotation <- Point.rotations
  , let delta = x - rotation y
  , let transform = (+ delta) . rotation
  , let pointCloud = Scanner.beacons $ transform `Scanner.map` scanner
  , PointCloud.size (beacons network `PointCloud.intersection` pointCloud) >= 12
  ]
  where
    candidates = IntMap.elems $ IntMap.intersectionWith (,) (differenceMap network) (Scanner.differenceMap scanner)

