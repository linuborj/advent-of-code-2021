module IntPQueue
  ( IntPQueue
  , empty
  , insert
  , null
  , minView
  ) where

import Prelude hiding (null)
import qualified Prelude
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


newtype IntPQueue item = IntPQueue (IntMap [item])
  deriving (Show, Eq, Ord)

empty :: IntPQueue item
empty = IntPQueue IntMap.empty

insert :: (Int, item) -> IntPQueue item -> IntPQueue item
insert (priority, item) (IntPQueue queue) = IntPQueue . IntMap.insertWith (++) priority [item] $ queue

minView :: IntPQueue item -> Maybe ((Int, item), IntPQueue item)
minView (IntPQueue queue) = do
  ((priority, item:items), queue') <- IntMap.minViewWithKey queue
  let queue'' = if Prelude.null items
                then queue'
                else IntMap.insert priority items queue'
  return ((priority, item), IntPQueue queue'')

null :: IntPQueue item -> Bool
null (IntPQueue queue) = IntMap.null queue

