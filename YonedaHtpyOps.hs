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

e2PCGrating 0 = error "grating of 0" 
e2PCGrating g = grating $ fst $ head $ toAList g

data YonedaData = YD (Map.Map E2Gen (Map.Map E2Gen Z2FreeSteenrodVS)) deriving (Ord,Show,Eq)

e2GenToChainMap :: Array Int [SteenrodAlgebra] -> E2GenData -> E2Gen -> Map.Map E2Gen (Z2FreeSteenrodVS)
e2GenToChainMap scb dta g = res
  where res = Map.fromList $ (g,toFModule unit):(zipMap ((antiDifferential scb dta).
                                                      (induceLinear $ induceMap res).
                                                      (differential dta).
                                                      rtensor.toFModule)
                                                 (filter (\h -> grating h > grating g) $ knownGens dta))

makeYonedaData scb dta = YD (Map.fromList $ zipMap (e2GenToChainMap scb dta) $ knownGens dta)

genMult :: ASSData -> E2Gen -> E2Gen -> Maybe E2PageConst
genMult (ASSData dta (YD mp) _) g1 g2 = if s <= (largestDegree dta) && t_s +1 < (largestDegree dta)
                            then Just $ sum $ map (\(p,_) -> if 0 == (induceLinear (induceMap h2) $ fromJust $ Map.lookup p h1)
                                                             then 0 else toFModule p)
                                 $ Map.toList $ (gensDiffMap dta)!(s,t_s)
                                                                                                                                                                        
                            else Nothing
  where (Just h1) = Map.lookup g1 mp
        (Just h2) = Map.lookup g2 mp
        s = grating g1 + grating g2
        t_s = degree g1 + degree g2

gensMult :: ASSData -> E2PageConst -> E2PageConst -> Maybe E2PageConst
gensMult dta x y = fmap sum $ mapM (uncurry $ genMult dta) [(a,b) | (a,1) <- toAList x, (b,1) <- toAList y]

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
                              

data HtpyOpData = HOD (Array Int (Map.Map E2Gen FreeSteenrodTensor)) deriving (Eq,Show,Ord)


-- we want N(x)1 to be a nullhomotopy
-- that means d.N(x)1 + N(x)1.d = 1(x)1
-- we have d .N(x)1 + N(x)1.d = (d(x)1+1(x)d)(N(x)1) + N(x)1(d(x)1+1(x)d)
-- = dN(x)1+N(x)d + Nd(x)1+N(x)d = (Nd+dN)(x)1 = 1(x)1


-- one worries that this may be brokded
makeHtpyOpData scb dta = HOD arr
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

hos = (loadE2Page 20) >>= (\it -> return $ makeHtpyOpData (admisArray 20) it)

-- Does sq_i :: E^{s,t-s} -> E^{2s-i,2t-2s-i}?  I think so, yes, I do think so.  
cup_iProd (ASSData dta _ (HOD arr)) i gn@(E2Gen s t_s pms) = 
  sum $ map toFModule $  filter ((1==).(coefOf (Tensor (Tensor unit gn) (Tensor unit gn)))
                                 .(\g -> fromJust $ Map.lookup g $ arr!i)) $ map fst $ Map.toList $ (gensDiffMap dta)!(2*s-i,2*t_s+i)


data ASSData = ASSData E2GenData YonedaData HtpyOpData deriving (Eq,Show)

makeASSData dta = ASSData dta (makeYonedaData scb dta) (makeHtpyOpData scb dta)
  where scb = admisArray $ largestDegree dta
        
gensAt (ASSData dta _ _) (s,t_s) = map fst $ Map.toList $ (gensDiffMap dta)!(s,t_s)