module Packet
  ( Packet (..)
  , parse
  ) where

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Control.Monad as Monad


data Packet
  = LiteralPacket
    { version :: Int
    , value :: Int
    }
  | OperatorPacket
    { version :: Int
    , operator :: Int
    , subpackets :: [Packet]
    }
  deriving (Show, Eq, Ord)


data Bit = Zero | One
  deriving (Show, Eq, Ord, Enum)

int :: [Bit] -> Int
int = sum . zipWith (flip (*)) exponents . bits
  where
    exponents = [2^n | n <- [0..]]
    bits = map (\b -> if b == Zero then 0 else 1) . reverse

bitParser :: ReadP Bit
bitParser = foldl1 (ReadP.<++)
  [ ReadP.char '0' >> return Zero
  , ReadP.char '1' >> return One
  ]

bitsParser :: Int -> ReadP [Bit]
bitsParser n = ReadP.count n bitParser

intParser :: Int -> ReadP Int
intParser n = int <$> bitsParser n

versionParser :: ReadP Int
versionParser = intParser 3

literalParser :: ReadP Int
literalParser = int . concat <$> groups
  where
    groups = foldl1 (ReadP.<++)
      [ (:) <$> notLastGroup <*> groups
      , (:[]) <$> lastGroup
      ]
    notLastGroup = ReadP.char '1' *> bitsParser 4
    lastGroup = ReadP.char '0' *> bitsParser 4

parseSection :: Show a => Int -> ReadP a -> ReadP a
parseSection n parser = do
  bits <- ReadP.count n (ReadP.satisfy (`elem` "01"))
  case parser `ReadP.readP_to_S` bits of
    [(parsed, "")] -> return parsed
    _              -> ReadP.pfail

asManyAsPossible :: ReadP a -> ReadP [a]
asManyAsPossible parser = foldl1 (ReadP.<++)
  [ (:) <$> parser <*> asManyAsPossible parser
  , return []
  ]

subPacketsParser :: ReadP [Packet]
subPacketsParser = do
  lengthType <- bitParser
  case lengthType of
    Zero -> do
      lengthInBits <- intParser 15
      lengthInBits `parseSection` asManyAsPossible packetParser
    One -> do
      numberOfPackets <- intParser 11
      numberOfPackets `ReadP.count` packetParser

literalPacketParser :: ReadP Packet
literalPacketParser = do
  version <- versionParser
  typ <- intParser 3
  Monad.guard $ typ == 4
  LiteralPacket version <$> literalParser

operatorPacketParser :: ReadP Packet
operatorPacketParser = do
  version <- versionParser
  operator <- intParser 3
  Monad.guard $ operator /= 4
  OperatorPacket version operator <$> subPacketsParser

packetParser :: ReadP Packet
packetParser = foldl1 (ReadP.<++)
  [ literalPacketParser
  , operatorPacketParser
  ]

dehex :: String -> String
dehex "" = ""
dehex (x:xs) = dehex' x ++ dehex xs
  where
    dehex' '0' = "0000"
    dehex' '1' = "0001"
    dehex' '2' = "0010"
    dehex' '3' = "0011"
    dehex' '4' = "0100"
    dehex' '5' = "0101"
    dehex' '6' = "0110"
    dehex' '7' = "0111"
    dehex' '8' = "1000"
    dehex' '9' = "1001"
    dehex' 'A' = "1010"
    dehex' 'B' = "1011"
    dehex' 'C' = "1100"
    dehex' 'D' = "1101"
    dehex' 'E' = "1110"
    dehex' 'F' = "1111"
    dehex' c   = error "'" ++ c:"' is not a hex literal"

parse :: String -> Packet
parse hexes = do
  case (packetParser <* ReadP.munch (== '0')) `ReadP.readP_to_S` dehex hexes of
    [(packet, "")] -> packet
    _              -> error "could not parse packet unambiguously"

