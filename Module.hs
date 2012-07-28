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
  (<**>) :: m -> m -> FreeModule m Integer
  x <**> y = toFModule $ x <*> y


data FreeModule m r = FM (Map.Map m r) deriving (Eq,Ord)

toAList :: FreeModule m r -> [(m,r)]
toAList (FM v) = Map.toList v

fromAList :: (Ord m, Num r, Eq r) => [(m,r)] -> FreeModule m r
fromAList ls  = cleanZerosFM $ FM $ foldr (\(m,r) mp -> Map.insertWith (+) m r mp) Map.empty ls

vmap :: (Ord m, Ord m', Num r, Num r', Eq r, Eq r')
        => ((m,r) -> (m',r')) -> FreeModule m r -> FreeModule m' r'
vmap f = fromAList.(map f).toAList

emap :: (Ord m, Ord m', Num r,  Eq r)
        => (m -> m') -> FreeModule m r -> FreeModule m' r
emap f = vmap (\(x,y) -> (f x,y)) 

smap :: (Multiplicative m, Multiplicative m', Num r, Eq r)
        => (m -> FreeModule m' r) -> FreeModule m r -> FreeModule m' r
smap f v = sum $ map (\(g,r) -> r *> (f g)) $ toAList v

rmap f v = vmap (\(x,y) -> (x,f y)) v

cleanZerosFM :: (Num r, Ord m, Eq r) => FreeModule m r -> FreeModule m r
cleanZerosFM (FM v) = FM $ Map.filter (/=0) v

isHomogenious :: FreeModule m r -> Bool
isHomogenious (FM v) = Map.size v == 1

coefOf x (FM v) = case Map.lookup x v of
  Nothing -> 0
  (Just y) -> y

toFModule :: (Ord m, Num r, Eq r) => m -> FreeModule m r
toFModule m = fromAList [(m,1)]

fromFModule :: FreeModule m r -> (m,r)
fromFModule v
  | isHomogenious v = head $ toAList v
  | otherwise       = error "Cannot call fromFModule on nonHomogenious vectors"

instance (Eq r, Ord m, Num r) => Module (FreeModule m r) r where
  r *> v = rmap (r*) v

instance (Multiplicative m, Num r, Eq r, Show r) => Show (FreeModule m r) where
  show 0 = "0" 
  show v      = cim " + " (\(g,r) -> (rrshow r) ++ (rshow g)) $ toAList v
    where rrshow r = if r == 1 then "" else rshow r

instance (Multiplicative m, Num r, Eq r) => Num (FreeModule m r) where
  (FM v1) + (FM v2) = cleanZerosFM $ FM $ Map.unionWith (+) v1 v2
  negate = rmap negate
  v1 * v2 = sum $ [(k1*k2) *> (rmap fromInteger $ g1<**> g2)
                             | (g1,k1) <- toAList v1, (g2,k2) <- toAList v2]
  fromInteger 0 = FM $ Map.empty
  fromInteger n = fromAList [(unit,fromInteger n)]
  abs = dumbError
  signum = dumbError


