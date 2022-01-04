module Main where

import Paths_day18
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import qualified Control.Monad as Monad
import Tree (Tree (..))
import qualified Tree


asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

intParser :: ReadP Int
intParser = do
  digits <- asManyAsPossible (ReadP.satisfy Char.isDigit)
  Monad.guard $ digits /= ""
  return $ read digits

inputParser :: ReadP [Tree Int]
inputParser = do
  trees <- asManyAsPossible (Tree.parser intParser <* ReadP.char '\n')
  ReadP.skipSpaces
  return trees

parser :: Show a => ReadP a -> String -> a
parser parser input = case ReadP.readP_to_S parser input of
  [(parsed, "")] -> parsed
  rest          -> error $ show rest

readInput :: IO [Tree Int]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . parser inputParser $ content

stabilized :: Eq a => (a -> a) -> a -> a
stabilized f x = fst . head . dropWhile (uncurry (/=)) . zip xs $ tail xs
  where
    xs = iterate f x

data Explode a
  = NotExploded (Tree a)
  | Exploded (Tree a) (a -> a) (a -> a)

explode :: (Eq a, Num a) => Tree a -> Tree a
explode = stabilized (extract . exp 0)
  where
    extract (NotExploded tree) = tree
    extract (Exploded tree _ _) = tree
    -- we can not explode a leaf
    exp _ (Leaf x) = NotExploded (Leaf x)
    -- we can explode a tree that consist of only two leaves, given that we are deep enough
    exp d tree@(Tree (Leaf l) (Leaf r))
      | d > 3 = Exploded (Leaf 0) (+l) (+r)
      | otherwise = NotExploded tree
    -- we can not explode a deep tree, but we can propagate the explosions in the branches
    exp d tree@(Tree l r) = case exp (d+1) l of
      -- if the left branch exploded, then ensure that we propagate the explortion to the right branch
      -- and ensure that nothing will be propagated to the right further up the tree
      Exploded l' f g -> Exploded (Tree l' (g `Tree.mapL` r)) f id
      -- if the left branch did not explode, check if the right one will
      _ -> case exp (d+1) r of
        -- if it does, then propagate the explotion to the left branch
        -- and ensure that nothing will be propagated to the left further up the tree
        Exploded r' f g -> Exploded (Tree (f `Tree.mapR` l) r') id g
        -- nothing exploded :(
        _ -> NotExploded tree

data Split a
  = NotSplit (Tree a)
  | Split (Tree a)

split :: Integral a => Tree a -> Tree a
split = extract . spl
  where
    extract (NotSplit tree) = tree
    extract (Split tree) = tree
    spl (Leaf n)
      | n > 9 = Split $ Tree (Leaf $ n `div` 2) (Leaf $ (n + 1) `div` 2)
      | otherwise = NotSplit $ Leaf n
    spl (Tree l r) = case spl l of
      Split l' -> Split $ Tree l' r
      _ -> case spl r of
        Split r' -> Split $ Tree l r'
        _ -> NotSplit $ Tree l r

reduce :: (Eq a, Integral a) => Tree a -> Tree a
reduce = stabilized (split . explode)

magnitude :: Num a => Tree a -> a
magnitude (Leaf x) = x
magnitude (Tree l r) = 3 * magnitude l + 2 * magnitude r

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . magnitude . foldl1 (\l -> reduce . Tree l) $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print $ maximum [magnitude . reduce $ Tree l r | l <- input, r <- input]

main :: IO ()
main = do
  part1 -- prints 'Part1: 3305'
  part2 -- prints 'Part2: 4563'

