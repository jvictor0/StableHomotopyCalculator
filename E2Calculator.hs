module E2Calculator where

import Module
import SteenrodAlgebra
import E2Gen
import qualified Data.Map as Map
import Data.Array.Unboxed
import qualified Data.Array as A
import Tensor
import LinearAlgebra
import Z2MatrixOps

data E2GenData = E2GD {serreCartanBasis :: A.Array Int [SteenrodAlgebra],
                       largestDegree :: Int,
                       gensDiffMap :: A.Array (Int,Int) (Map.Map E2Gen Z2FreeSteenrodVS)
                      }

gensUpToMap :: E2GenData -> Int -> Int -> Map.Map E2Gen Z2FreeSteenrodVS
gensUpToMap dt s t_s = Map.unions $ map (\t_s' -> (gensDiffMap dt)!(s,t_s')) [0..t_s]


z2Basis :: E2GenData -> Int -> Map.Map E2Gen a -> Basis (Tensor SteenrodSquare E2Gen)
z2Basis dta t_s mp = array (1,length lst) $ zip [1..] lst
  where lst = concatMap (\g -> map (\sq -> Tensor (fst $ fromFModule sq) g)
                               $ (serreCartanBasis dta)!(t_s-(degree g))) $ map fst $ Map.toList mp

specialE2At _ _ _ = Nothing


extraGensAt :: E2GenData -> Int -> Int -> Map.Map E2Gen Z2FreeSteenrodVS
extraGensAt dta s t_s = case specialE2At dta s t_s of
  Nothing -> Map.empty
  (Just res) -> res
  where lmp =  gensUpToMap dta s (t_s-1)
        cmp =  gensUpToMap dta (s-1) t_s
        ldom = z2Basis dta t_s lmp 
        cdom = z2Basis dta (t_s+1) cmp
        rdom = z2Basis dta (t_s+2) $ gensUpToMap dta (s-2) t_s
        lmatrix = toMatrix ldom cdom $ induceLinear lmp
        cmatrix = toMatrix cdom rdom $ induceLinear cmp
        (imag,_,rank,_) = imageKernel lmatrix
        (_,kern,_,nullity) = imageKernel cmatrix
        