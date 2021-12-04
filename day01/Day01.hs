module Main where

factorial :: Integral a => a -> a
factorial n = product [1..n]

main :: IO()
main = print . sum . map (read . (:[])) . show $ factorial 100

