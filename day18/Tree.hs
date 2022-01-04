module Tree
  ( Tree (..)
  , parser
  , mapL
  , mapR
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP


data Tree a
  = Tree (Tree a) (Tree a)
  | Leaf a
  deriving (Eq, Ord)

instance Show a => Show (Tree a) where
  show (Leaf x)   = show x
  show (Tree l r) = "[" ++ show l ++ "," ++ show r ++ "]"

treeParser :: ReadP a -> ReadP (Tree a)
treeParser leafParser = do
  ReadP.char '['
  left <- parser leafParser
  ReadP.char ','
  right <- parser leafParser
  ReadP.char ']'
  return $ Tree left right

parser :: ReadP a -> ReadP (Tree a)
parser leafParser = foldl1 (ReadP.<++)
  [ Leaf <$> leafParser
  , treeParser leafParser
  ]

depth :: Tree a -> Integer
depth (Leaf _)   = 1
depth (Tree l r) = max (depth l) (depth r)

mapL :: (a -> a) -> Tree a -> Tree a
mapL f (Leaf x) = Leaf $ f x
mapL f (Tree l r) = Tree (mapL f l) r

mapR :: (a -> a) -> Tree a -> Tree a
mapR f (Leaf x) = Leaf $ f x
mapR f (Tree l r) = Tree l (mapR f r)

