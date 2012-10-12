module EqClasses where

import Data.Array
import Control.Monad.ST
import qualified Data.Set as Set
import Data.List

eqClasses :: (Ord a) => [Set.Set a] -> [Set.Set a]
eqClasses [] = []
eqClasses (fst:rst) = if null same then fst:(eqClasses rst) else eqClasses $ (Set.unions $ fst:same):diff
  where (same,diff) = partition (\s -> not $ 0==(Set.size $ Set.intersection s fst)) rst