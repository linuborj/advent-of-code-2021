module Number
  ( Number (..)
  , isMarked
  , value
  ) where


data Number
  = Marked Int
  | Unmarked Int
  deriving Eq

isMarked :: Number -> Bool
isMarked (Marked _) = True
isMarked _          = False

value :: Number -> Int
value (Marked x) = x
value (Unmarked x) = x

pad xs
  | null xs        = "  "
  | length xs == 1 = ' ':xs
  | otherwise      = xs

instance Show Number where
  show (Unmarked n) = pad $ show n
  show (Marked n) = concatMap strikethrough . pad $ show n
    where
      strikethrough '0' = "0̶"
      strikethrough '1' = "1̶"
      strikethrough '2' = "2̶"
      strikethrough '3' = "3̶"
      strikethrough '4' = "4̶"
      strikethrough '5' = "5̶"
      strikethrough '6' = "6̶"
      strikethrough '7' = "7̶"
      strikethrough '8' = "8̶"
      strikethrough '9' = "9̶"
      strikethrough c   = [c]


