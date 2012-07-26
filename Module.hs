{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Module where

import qualified Data.Map as Map
import Utils

class (Num r) => Module m r where
 (*>) :: r -> m -> m






-------------------------------------------
---  Free Module data type and functions --
-------------------------------------------

class (Ord m, Show m) => Multiplicative m where
  unit :: m
  (<*>) :: m -> m -> m
  _ <*> _ = error "<*> not implemented, which is not required"
  (<**>) :: (Num r) => m -> m -> FreeModule m r
  x <**> y = toFModule $ x <*> y


data FreeModule m r = FM (Map.Map m r) deriving (Eq,Ord)

toAList :: FreeModule m r -> [(m,r)]
toAList (FM v) = Map.toList v

fromAList :: (Ord m) => [(m,r)] -> FreeModule m r
fromAList = FM.(Map.fromList)

cleanZerosFM :: (Num r, Ord m, Eq r) => FreeModule m r -> FreeModule m r
cleanZerosFM (FM v) = FM $ Map.filter (/=0) v

isHomogenious :: FreeModule m r -> Bool
isHomogenious (FM v) = Map.size v == 1

toFModule :: (Multiplicative m, Num r) => m -> FreeModule m r
toFModule m = fromAList [(m,1)]

fromFModule :: FreeModule m r -> (m,r)
fromFModule v
  | isHomogenious v = head $ toAList v
  | otherwise       = error "Cannot call fromFModule on nonHomogenious vectors"

instance (Num r) => Module (FreeModule m r) r where
  r *> v = fmap (r*) v

instance Functor (FreeModule m) where
  fmap f (FM v) = FM $ fmap f v

instance (Multiplicative m, Num r, Eq r, Show r) => Show (FreeModule m r) where
  show v = cim " + " (\(g,r) -> (rrshow r) ++ (rshow g)) $ toAList v
    where rrshow r = if r == 1 then "" else rshow r

instance (Multiplicative m, Num r, Eq r) => Num (FreeModule m r) where
  (FM v1) + (FM v2) = cleanZerosFM $ FM $ Map.unionWith (+) v1 v2
  negate = fmap negate
  v1 * v2 = sum $ [(k1*k2) *> (g1<**> g2)
                             | (g1,k1) <- toAList v1, (g2,k2) <- toAList v2]
  fromInteger 0 = FM $ Map.empty
  fromInteger n = fromAList [(unit,fromInteger n)]
  abs = dumbError
  signum = dumbError


