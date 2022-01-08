{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
module Point where

import Control.Applicative (liftA2)

data Point a = Point a a a
  deriving (Show, Eq, Ord)

instance Functor Point where
  fmap f (Point x y z) = Point (f x) (f y) (f z)
  x <$ _ = Point x x x

instance Foldable Point where
  foldMap f (Point x y z) = f x <> f y <> f z

instance Applicative Point where
  pure x = Point x x x
  Point f g h <*> Point x y z = Point (f x) (g y) (h z)

instance Num a => Num (Point a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Enum (Point Int) where
  fromEnum (Point x y z) = 100_000_000 * (x + 5000) + 10_000 * (y + 5000) + (z + 5000)
  toEnum xyz = Point (x - 5000) (y - 5000) (z - 5000) 
    where
      (x, yz) = xyz `divMod` 100_000_000
      (y, z) = yz `divMod` 10_000

rotations :: Num a => [Point a -> Point a]
rotations =
  [ cycle . flip . negation
  | cycle <- cycles
  , flip <- flips
  , negation <- negations
  ]
  where
    cycles =
      [ \(Point x y z) -> Point x y z
      , \(Point x y z) -> Point y z x
      , \(Point x y z) -> Point z x y
      ]
    flips =
      [ \(Point x y z) -> Point x y z
      , \(Point x y z) -> Point (-x) (-y) z
      , \(Point x y z) -> Point (-x) y (-z)
      , \(Point x y z) -> Point x (-y) (-z)
      ]
    negations =
      [ \(Point x y z) -> Point x y z
      , \(Point x y z) -> Point (-z) (-y) (-x)
      ]

