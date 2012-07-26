{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Module where

import qualified Data.Map as Map

class (Num r) => Module m r where
 (*>) :: r -> m -> m

class (Ord m) => Multiplicative m where
  unit :: m
  (<*>) :: m -> m -> m
  _ <*> _ = error "<*> not implemented, which is not required"
  (<**>) :: (Num r) => m -> m -> FreeModule m r
  x <**> y = toModule $ x <*> y

data FreeModule m r = FM (Map.Map m r)

toAList :: FreeModule m r -> [(m,r)]
toAList (FM v) = Map.toList v

fromAList :: (Ord m) => [(m,r)] -> FreeModule m r
fromAList = FM.(Map.fromList)

cleanZerosFM :: (Num r) => FreeModule m r -> FreeModule m r
cleanZerosFM (FM v) = FM $ Map.filter (==0) v

isHomogenious :: FreeModule m r -> Bool
isHomogenious (FM v) = Map.size v == 1

toFModule :: (Multiplicative m, Num r) => m -> FreeModule m r
toFModule m = fromAList [(m,1)]

fromFModule :: FreeModule m r -> (m,r)
fromFModule v
  | isHomogenious v = head $ toAList v
  | otherwise       = error "Cannot call fromFModule on nonHomogenious vectors"

