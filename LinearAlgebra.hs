{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module LinearAlgebra where

import Module
import Utils
import qualified Data.Map as Map
import Data.Array.Unboxed

type Basis m = UArray Int m
type Matrix r = UArray (Int,Int) r
type Vector m = UArray Int m

linearMap ::(Multiplicative m, Multiplicative m', Num r, Eq r) 
            => (Map.Map m (FreeModule m' r)) -> FreeModule m r -> FreeModule m' r
linearMap mp v = smap (\x -> case Map.lookup x mp of
                          Nothing -> 0
                          (Just y) -> y)
                 v
            
-- f is assumed to be linear here
toMatrix :: (Multiplicative m, Multiplicative m', Num r, Eq r,
             IArray UArray r, IArray UArray m, IArray UArray m')
            => Basis m -> Basis m' -> (FreeModule m r -> FreeModule m' r) -> Matrix r
toMatrix dom codom f = array ((a,b),(c,d)) [((i,j),coefOf (codom!j) fi)
                                           | i <- [a..c], j <- [b..d], fi <- [f $ toFModule $ dom!i]]
  where (a,c) = bounds dom
        (b,d) = bounds codom


decompose domain v = amap (flip coefOf v) domain

recompose domain v = fromAList $ map (\(i,r) -> (domain!i,r)) $ assocs v

