module Board
  ( Board (..)
  , mark
  , isWinner
  , score
  ) where

import Number
import qualified Data.List as List


newtype Board = Board { rows :: [[Number]] }
  deriving Eq

instance Show Board where
  show (Board rows) = unlines . map (unwords . map show) $ rows

mark :: Board -> Int -> Board
mark (Board rows) n = Board $ map (map mark') rows
  where
    mark' (Marked m) = Marked m
    mark' (Unmarked m)
      | n == m    = Marked m
      | otherwise = Unmarked m

isWinner :: Board -> Bool
isWinner (Board rows) = any (all isMarked) $ rows ++ columns
  where
    isMarked (Marked _) = True
    isMarked _          = False
    columns = List.transpose rows

score :: Board -> Int -> Maybe Int
score board@(Board rows) lastPlayed
  | isWinner board = Just . (* lastPlayed) . sum . map value . filter (not . isMarked) . concat $ rows
  | otherwise      = Nothing


