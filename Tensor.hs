{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Tensor where

import BiFunctor
import Module
import Utils
import LinearAlgebra
import qualified Data.Map as Map
import Utils

data Tensor a b = Tensor a b deriving (Eq,Ord)

instance (Show a, Show b) => Show (Tensor a b) where
  show (Tensor a b) = (rshow a) ++ "(x)" ++ (rshow b)

instance (UnShow a, UnShow b) => UnShow (Tensor a b) where
  unShow str = let (a,b) = splitSubstr "(x)" str in Tensor (unShow a) (unShow b)

instance (Multiplicative a, Multiplicative b) => Multiplicative (Tensor a b) where
  unit = Tensor unit unit
  (Tensor a b) <**> (Tensor c d) = fromAList [(Tensor x y,k1*k2)
                                             | (x,k1) <- toAList $ a <**> c,
                                               (y,k2) <- toAList $ b <**> d]

tensorFlip v = emap (\(Tensor x y) -> Tensor y x) v

ltensor :: (Eq r, Num r, Multiplicative m1, Multiplicative m2)
           => FreeModule m1 r -> FreeModule (Tensor m1 m2) r
ltensor v = vmap (\(x,r) -> (Tensor x unit,r)) v

rtensor :: (Eq r, Num r, Multiplicative m1, Multiplicative m2)
           => FreeModule m1 r -> FreeModule (Tensor m2 m1) r
rtensor = tensorFlip.ltensor

tensor x y = (ltensor x)*(rtensor y)

instance BiFunctor Tensor where
  bmap f g (Tensor x y)  = Tensor (f x) (g y)

ten :: (Multiplicative m1, Multiplicative m2, Multiplicative a, Multiplicative b, Num r, Eq r)
       => (FreeModule a r -> FreeModule m1 r) -> (FreeModule b r -> FreeModule m2 r)
       -> FreeModule (Tensor a b) r -> FreeModule (Tensor m1 m2) r
(f `ten` g) v = smap (\(Tensor a b) -> tensor (f $ toFModule a) (g $ toFModule b)) v



--- we make use of the isomorphism where, if M is R-free and R is a k-algebra,
--- then M ~= M (x)_R R as a free k-vectorspace, ei, the free k-vectorspace on
--- the simple tensors of the basis of R-basis of M with the k-basis of R is
--- M as a k-vectorspace

induceStructure ::(Eq k, Num k, Multiplicative r, Multiplicative m)
                  => (FreeModule m (FreeModule r k)) -> (FreeModule (Tensor r m) k)
induceStructure v = sum $ map (\(m,a) -> tensor a (toFModule m)) $ toAList v

reduceStructure  :: (Eq r, Ord m, Multiplicative m', Num r)
                    => FreeModule (Tensor m' m) r -> FreeModule m (FreeModule m' r)
reduceStructure v = vmap (\(Tensor r m,k) -> (m,k*>(toFModule r))) v

induceLinear mp v = induceStructure $ linearMap (Map.map reduceStructure mp) $ reduceStructure v

