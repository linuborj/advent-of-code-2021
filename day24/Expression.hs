module Expression where

import Register (Register (..))
import Text.Subscript (subscript)


data Expression
  = Expression :+ Expression
  | Expression :* Expression
  | Expression :% Expression
  | Expression :/ Expression
  | Expression := Expression
  | Reference Register Int
  | Input Register Int
  | Number Int
  deriving (Eq, Ord)

isLiteral :: Expression -> Bool
isLiteral (Number _) = True
isLiteral (Reference _ _) = True
isLiteral (Input _ _) = True
isLiteral _ = False

instance Show Expression where
  show exp = display exp
    where
      display (Number n) = show n
      display (Input register version) = show register ++ subscript version ++ "ⁱ"
      display (Reference register version) = show register ++ subscript version
      display (exp0 :+ exp1) = displayArg exp0 ++ "+" ++ displayArg exp1
      display (exp0 :* exp1) = displayArg exp0 ++ "*" ++ displayArg exp1
      display (exp0 :/ exp1) = displayArg exp0 ++ "/" ++ displayArg exp1
      display (exp0 :% exp1) = displayArg exp0 ++ "%" ++ displayArg exp1
      display (exp0 := exp1) = displayArg exp0 ++ "=" ++ displayArg exp1
      displayArg exp
        | isLiteral exp = display exp
        | otherwise = "(" ++ display exp ++ ")"

reduce :: Expression -> Expression
reduce exp = snd . head . dropWhile (uncurry (/=)) $ zip exps (tail exps)
  where
    exps = iterate red exp
    red (exp0 :* Number 0) = Number 0
    red (Number 0 :* exp1) = Number 0
    red (exp0 :* Number 1) = exp0
    red (Number 1 :* exp1) = exp1
    red (exp0 :/ Number 1) = exp0
    red (Number 1 :/ exp1) = exp1
    red (exp0 :+ Number 0) = exp0
    red (Number 0 :+ exp1) = exp1
    red (exp0 :% Number 1) = Number 0
    red (Number 0 :% exp1) = Number 0
    red (Number n :+ Number m) = Number $ n + m
    red (Number n :* Number m) = Number $ n * m
    red (Number n :/ Number m) = Number $ n `div` m
    red (Number n :% Number m) = Number $ n `mod` m
    red (exp0 :+ exp1) | exp0 == exp1 = Number 1
    red (exp0 :* exp1) = red exp0 :* red exp1
    red (exp0 :+ exp1) = red exp0 :+ red exp1
    red (exp0 :% exp1) = red exp0 :% red exp1
    red (exp0 :/ exp1) = red exp0 :/ red exp1
    red (exp0 := exp1) = red exp0 := red exp1
    red (Input register version) = Input register version
    red (Reference register version) = Reference register version
    red (Number n) = Number n

