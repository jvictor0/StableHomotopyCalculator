{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module LinearAlgebra where

import Module
import Utils
import qualified Data.Map as Map
import Data.Array.Unboxed
import qualified Data.Array as A
import ZMod2

type Basis m = A.Array Int m
type Matrix r = UArray (Int,Int) r
type Vector m = UArray Int m

linearMap ::(Multiplicative m, Multiplicative m', Num r, Eq r) 
            => (Map.Map m (FreeModule m' r)) -> FreeModule m r -> FreeModule m' r
linearMap mp v = smap (\x -> case Map.lookup x mp of
                          Nothing -> 0
                          (Just y) -> y)
                 v
            
-- f is assumed to be linear here
-- BE SUSPUSION OF THE RUNETIME OF THIS FUNCTIONS!!!!!!
toMatrix :: (Multiplicative m, Multiplicative m', Num r, Eq r,
             IArray UArray r)
            => Basis m -> Basis m' -> (FreeModule m r -> FreeModule m' r) -> Matrix r
toMatrix dom codom f = array ((a,b),(c,d)) [((i,j),coefOf (codom!j) fi)
                                           | i <- [a..c], j <- [b..d], fi <- [f $ toFModule $ dom!i]]
  where (a,c) = bounds dom
        (b,d) = bounds codom

decompose :: (IArray Array x,IArray UArray r,Num r, Eq r, Ord x) => Basis x -> FreeModule x r -> Vector r
decompose domain v = array (bounds domain) $ map (\(x,y) -> (x,coefOf y v)) $ assocs domain

recompose :: (IArray Array x,IArray UArray r,Num r, Eq r, Ord x) => Basis x -> Vector r -> FreeModule x r 
recompose domain v = fromAList $ map (\(i,r) -> (domain!i,r)) $ assocs v

vlistToMatrix ::  (IArray UArray x) => [Vector x] -> Matrix x
vlistToMatrix lst = array ((0,a),(length lst-1,b)) 
                    $ concat $ zipWith  (\i ar -> [((i,j), ar ! j) | j <- [a..b]]) [0..length lst -1] lst
  where (a,b) = bounds $ head lst
        
