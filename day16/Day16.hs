module Main where

import Paths_day16
import Packet (Packet)
import qualified Packet


cleanInput :: String -> String
cleanInput = concat . lines -- remove newlines

readInput :: IO Packet
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Packet.parse . cleanInput $ content

versionSum :: Packet -> Int
versionSum (Packet.LiteralPacket version _) = version
versionSum (Packet.OperatorPacket version _ subpackets) = foldl (+) version (map versionSum subpackets)

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print $ versionSum input

eval :: Packet -> Int
eval (Packet.LiteralPacket _ value) = value
eval (Packet.OperatorPacket _ operator subpackets) = case operator of
  0 -> sum $ map eval subpackets
  1 -> product $ map eval subpackets
  2 -> minimum $ map eval subpackets
  3 -> maximum $ map eval subpackets
  5 -> let [x, y] = map eval subpackets in if x > y then 1 else 0
  6 -> let [x, y] = map eval subpackets in if x < y then 1 else 0
  7 -> let [x, y] = map eval subpackets in if x == y then 1 else 0
  _ -> error $ "unknown operator " ++ show operator

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print $ eval input

main :: IO ()
main = do
  part1 -- prints 'Part1: 947'
  part2 -- prints 'Part2: 660797830937'

