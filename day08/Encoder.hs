module Encoder
  ( Encoder
  , encoder
  , encode
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Signal (Signal)
import qualified Signal
import qualified Data.List as List
import Digit (Digit)
import qualified Digit


-- | An encoder is a permutation of signals
newtype Encoder = Encoder (Map Signal Signal)
  deriving Show

-- | The list of possible encoders. Consists of the possible permutations of signals
encoders :: [Encoder]
encoders = map ((Encoder . Map.fromList) . zip [Signal.A .. Signal.G]) $ List.permutations [Signal.A .. Signal.G]

-- | Encodes the signals that make up a digit with the given encoder
encode :: Encoder -> Digit -> Set Signal
encode (Encoder mapping) = Set.map (mapping Map.!) . Digit.pattern

-- | Digit with the same length as the given pattern of signals
digits :: Set Signal -> [Digit]
digits pattern = filter ((length pattern ==) . length . Digit.pattern) [Digit.Zero .. Digit.Nine]

-- | A predicate that is true if there is a digit the when encoded is equal to the given signal pattern
encodes :: Encoder -> Set Signal -> Bool
encodes encoder pattern = any ((pattern ==) . encode encoder) $ digits pattern

-- | A measure of ambiguity
ambiguity :: Set Signal -> Int
ambiguity = length . digits

-- | Finds an encoder that can encode digits to produce the given list of signal patterns
encoder :: [Set Signal] -> Encoder
encoder patterns = head $ filter (\encoder -> all (encodes encoder) patterns') encoders
  where
    -- We want to apply the least ambiguous pattern first to reduce the search space
    -- as much as possible
    patterns' = List.sortOn ambiguity patterns

