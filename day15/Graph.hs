{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
module Graph
  ( Graph (..)
  , connectedGrid
  )where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Bifunctor as Bifunctor

data Graph node cost = Graph
  { nodes :: Set node
  , cost :: node -> cost
  , adjacent :: node -> Set node
  }

instance Enum (Int, Int) where
  toEnum = (`divMod` 1_000_000)
  fromEnum (x, y) = x * 1_000_000 + y

connectedGrid :: Num n => [[n]] -> Graph (Int, Int) n
connectedGrid xs = Graph
  { nodes    = Set.fromList . map fst $ nodes
  , cost     = (grid IntMap.!) . fromEnum
  , adjacent = adjacent
  }
  where
    nodes = concat . zipWith (\y -> zipWith ((,) . (, y)) [0..]) [0..] $ xs
    grid = IntMap.fromList . map (Bifunctor.first fromEnum) $ nodes
    adjacent (x, y) = Set.fromList . filter ((`IntMap.member` grid) . (fromEnum :: (Int, Int) -> Int)) $
      [ (x-1, y)
      , (x+1, y)
      , (x, y-1)
      , (x, y+1)
      ]

