{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module HopfStructure where

import Module
import SteenrodAlgebra
import E2Gen
import Utils
import Tensor
import AdditiveCat
import ZMod2

type FreeSteenrodTensor = (FreeModule (Tensor (Tensor SteenrodSquare E2Gen)
                                       (Tensor SteenrodSquare E2Gen)) ZMod2)
newtype HTenMod = HTM  FreeSteenrodTensor deriving (Eq, Ord)

fromHTM (HTM x) = x

instance Show HTenMod where
  show (HTM 0) = "0"
  show (HTM v) = cim " + " (\(Tensor (Tensor sq1 x1) (Tensor sq2 x2),k) -> (show $ (toFModule sq1 :: SteenrodAlgebra) *> (toFModule x1 :: FreeSteenrodModule))
                                                                           ++ "(x)"
                                                                           ++ (show $ (toFModule sq2 :: SteenrodAlgebra) *> (toFModule x2 :: FreeSteenrodModule)))
                 $ toAList v

coMult :: SteenrodAlgebra -> FreeModule (Tensor SteenrodSquare SteenrodSquare) ZMod2
coMult st = smap (\(Sq ls) -> product $ map (\i -> (sum $ map (\j -> toFModule $ Tensor (Sq[j]) (Sq[i-j])) [1..i-1]) +
                                                   (ltensor $ toFModule $ Sq[i]) + (rtensor $ toFModule $ Sq[i]))
                              ls) st

instance Module HTenMod SteenrodAlgebra where
  sq *> (HTM v) = HTM $ sum $ map (\(Tensor sq1 sq2,k) -> (smap (\(Tensor rq y) -> vmap (\(u,r) -> (Tensor u y,fromInteger r)) $ sq1<**>rq))`ten`
                                                          (smap (\(Tensor rq y) -> vmap (\(u,r) -> (Tensor u y,fromInteger r)) $ sq2<**>rq)) $ v) $ toAList $ coMult sq

