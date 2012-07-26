{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SteenrodAlgebra where

import Module
import Utils

data SteenrodSquare = Sq [Int] deriving (Eq, Ord)

instance Show SteenrodSquare where
  Show (Sq []) = "1"
  Show (Sq [x]) = "Sq^" ++ (texShow x)
  Show (Sq ls) = "Sq^{\\{" ++ (cim "," show ls ) ++ "\\}}"

type SteenrodAlgebra = FreeModule SteenrodSquare ZMod2

adem :: Int -> Int -> 