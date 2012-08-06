module YonedaHtpyOps where

import Module
import ZMod2
import Data.Maybe
import E2Gen
import E2Calculator
import qualified Data.Map as Map
import Module
import LinearAlgebra
import Tensor
import Utils
import Data.Array
import SteenrodAlgebra
import E2Saver
import HopfStructure
import Data.List

type E2PageConst = FreeModule E2Gen ZMod2

data YonedaData = YD (Map.Map E2Gen (Map.Map E2Gen Z2FreeSteenrodVS)) E2GenData deriving (Ord,Show,Eq)

e2GenToChainMap :: Array Int [SteenrodAlgebra] -> E2GenData -> E2Gen -> Map.Map E2Gen (Z2FreeSteenrodVS)
e2GenToChainMap scb dta g = res
  where res = Map.fromList $ (g,toFModule unit):(zipMap ((antiDifferential scb dta).
                                                      (induceLinear $ induceMap res).
                                                      (differential dta).
                                                      rtensor.toFModule)
                                                 (filter (\h -> grating h > grating g) $ knownGens dta))

makeYonedaData scb dta = YD (Map.fromList $ zipMap (e2GenToChainMap scb dta) $ knownGens dta) dta

genMult :: YonedaData -> E2Gen -> E2Gen -> Maybe E2PageConst
genMult (YD mp dta) g1 g2 = if s <= (largestDegree dta) && t_s +1 < (largestDegree dta)
                            then Just $ sum $ map (\(p,_) -> if 0 == (induceLinear (induceMap h2) $ fromJust $ Map.lookup p h1)
                                                             then 0 else toFModule p)
                                 $ Map.toList $ (gensDiffMap dta)!(s,t_s)
                                                                                                                                                                        
                            else Nothing
  where (Just h1) = Map.lookup g1 mp
        (Just h2) = Map.lookup g2 mp
        s = grating g1 + grating g2
        t_s = degree g1 + degree g2

yd = (loadE2Page 20) >>= (\it -> return $ makeYonedaData (admisArray 20) it)

-- question: does the memory cost of all the friggin pairs of steenrod ops and generators really make worth it the speedup?
-- we shall see how bad this is.  It seems like antiDifferential is not a horrid function
-- we worry about linearity, but this should be overkill
-- for nullhtpy h = d^{-1}(1+hd) 
nullHtpy _ _ 0 = 0
nullHtpy scb dta v
  | isHomogenious v = antiDifferential scb dta $ v + (nullHtpy scb dta $ differential dta v)
  | otherwise      = smap (\g -> nullHtpy scb dta $ toFModule g) v

tensorDifferential dta = ((differential dta)`ten`id) + (id`ten`(differential dta))
tensorNullHtpy scb dta = ((nullHtpy scb dta)`ten`id) + magicEps
  where magicEps v = smap (\g -> case g of
                              (Tensor (Tensor (Sq[]) (E2Gen 0 0 0)) c) -> tensor (toFModule unit) (nullHtpy scb dta $ toFModule c)
                              _               -> 0) v
                              

data HtpyOpData = HOD (Array Int (Map.Map E2Gen FreeSteenrodTensor)) E2GenData deriving (Eq,Show,Ord)


-- we want N(x)1 to be a nullhomotopy
-- that means d.N(x)1 + N(x)1.d = 1(x)1
-- we have d .N(x)1 + N(x)1.d = (d(x)1+1(x)d)(N(x)1) + N(x)1(d(x)1+1(x)d)
-- = dN(x)1+N(x)d + Nd(x)1+N(x)d = (Nd+dN)(x)1 = 1(x)1


-- one worries that this may be brokded
makeHtpyOpData scb dta = HOD arr dta
  where arr :: Array Int (Map.Map E2Gen FreeSteenrodTensor)
        arr = array (0,largestDegree dta) $
              [(j,Map.fromList $ (firstAt j) ++ (zipMap ((\g -> dm1 $ ((bigD j) $ differential dta g) +
                                                             ((1+tensorFlip)$ (bigD $ j-1) g)).
                                                         (rtensor.toFModule))
                                               $ (knownGens dta)\\(if j == 0 then [unit] else []))) | j <- [0..largestDegree dta]]
        firstAt 0 = [(unit,toFModule  unit)]
        firstAt _ = []
        d :: FreeSteenrodTensor -> FreeSteenrodTensor
        d = tensorDifferential dta
        dm1 = tensorNullHtpy scb dta
        bigD :: Int -> Z2FreeSteenrodVS -> FreeSteenrodTensor
        bigD (-1) _ = 0
        bigD j v = sum $ map (\(Tensor sq g,k) -> fromHTM $ (toFModule sq :: SteenrodAlgebra) *> (HTM $ lk0 g)) $ toAList v
          where lk0 g = case Map.lookup g (arr!j) of
                  Nothing -> 0
                  (Just x) -> x

hos = (loadE2Page 10) >>= (\it -> return $ makeHtpyOpData (admisArray 10) it)
doit i j  = hos >>= (\(HOD arr _) -> return $ Map.lookup (E2Gen i j 0) $ arr!0)
cup_i_test i s t_s = hos >>= (\h -> return $ cup_iProd h i (E2Gen s t_s 0))

-- Does sq_i :: E^{s,t-s} -> E^{2s-i,2t-2s-i}?  I think so, yes, I do think so.  
cup_iProd (HOD arr dta) i gn@(E2Gen s t_s pms) = 
  sum $ map toFModule $  filter ((1==).(coefOf (Tensor (Tensor unit gn) (Tensor unit gn)))
                                 .(\g -> fromJust $ Map.lookup g $ arr!i)) $ map fst $ Map.toList $ (gensDiffMap dta)!(2*s-i,2*t_s+i)

