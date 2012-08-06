module E2Calculator where

import Data.Maybe
import Data.Time
import Timing 
import Module
import SteenrodAlgebra
import E2Gen
import qualified Data.Map as Map
import Data.Array.Unboxed
import qualified Data.Array as A
import Tensor
import LinearAlgebra
import Z2MatrixOps
import Utils
import ZMod2

-- why is this slow?

data E2GenData = E2GD {largestDegree :: Int,
                       gensDiffMap :: A.Array (Int,Int) (Map.Map E2Gen Z2FreeSteenrodVS),
                       knownGens :: [E2Gen]
                      } deriving (Eq,Ord)

instance Show E2GenData where
  show dta = cim "\n" (\(x,y) -> (show x) ++ " |--> " ++ (show y)) $ concat $ map (Map.toList . snd) $ assocs $ gensDiffMap dta

gensUpToMap :: E2GenData -> Int -> Int -> Map.Map E2Gen Z2FreeSteenrodVS
gensUpToMap dt s t_s = Map.unions $ map (\t_s' -> (gensDiffMap dt)!(s,t_s')) [0..t_s]


z2Basis :: A.Array Int [SteenrodAlgebra] -> Int -> Map.Map E2Gen a -> Basis (Tensor SteenrodSquare E2Gen)
z2Basis serreCartanBasis t_s mp = array (1,length lst) $ zip [1..] lst
  where lst = concatMap (\g -> map (\sq -> Tensor (fst $ fromFModule sq) g)
                               $ serreCartanBasis!(t_s-(degree g))) $ map fst $ Map.toList mp

specialE2At dta 0 0   = Just $ Map.singleton unit 0
specialE2At dta s 0   = Just $ Map.singleton (E2Gen s 0 0) (toFModule $ Tensor (Sq[1]) (E2Gen (s-1) 0 0))
specialE2At dta 0 t_s = Just $ Map.empty
specialE2At dta 1 t_s = Just $ if (t_s+1) `isPowerOf` 2
                               then Map.singleton (E2Gen 1 t_s 0) $ toFModule $ Tensor (Sq[t_s+1]) unit
                               else Map.empty
specialE2At dta s t_s
  | largestDegree dta < t_s+2  = Just Map.empty
specialE2At _ _ _ = Nothing

-- we don't use makeMatrix because it's slightly not right
extraGensAt :: A.Array Int [SteenrodAlgebra] -> E2GenData -> Int -> Int -> Map.Map E2Gen Z2FreeSteenrodVS
extraGensAt scb dta s t_s = case specialE2At dta s t_s of
  Nothing -> Map.fromList $ zip (map (E2Gen s t_s) [0..]) newImagVects
  (Just res) -> res
  where lmp =  gensUpToMap dta s (t_s-1)
        cmp =  gensUpToMap dta (s-1) t_s
        ldom = z2Basis scb t_s lmp 
        cdom = z2Basis scb (t_s+1) cmp
        rdom = z2Basis scb (t_s+2) $ gensUpToMap dta (s-2) t_s
        lmatrix = induceMatrix ldom cdom lmp
        cmatrix = induceMatrix cdom rdom cmp
        (imag,_,rank,_) = imageKernel lmatrix
        (_,kern,_,nullity) = imageKernel cmatrix
        newPureVects = take (nullity-rank) $ filter (\v -> not $ v `vin` imag) kern
        newImagVects = map (recompose cdom) newPureVects

extendE2Data :: E2GenData -> Int -> E2GenData
extendE2Data dta_old ld = dta
  where dta = E2GD {largestDegree = ld,
                    gensDiffMap = array ((0,0),(ld,ld)) [((i,j),result i j) | i <- [0..ld], j <- [0..ld]],
                    knownGens = map fst $  concatMap (Map.toList . snd) $filter (\((i,j),x) -> j +1 < ld) $  assocs $ gensDiffMap dta}
        result i j = if i <= (largestDegree dta_old) && (j+1) < (largestDegree dta_old)
                     then (gensDiffMap dta_old)!(i,j)
                     else extraGensAt serreCartanBasis dta i j
        serreCartanBasis = admisArray ld
                          
calculateE2Page :: Int -> E2GenData
calculateE2Page ld = extendE2Data (E2GD {largestDegree = 0,
                                         gensDiffMap = array ((0,0),(0,0)) [],
                                         knownGens = []})
                     ld

-- just for convinience
makeMatrix :: E2GenData -> (Array Int [SteenrodAlgebra]) -> Int -> Int -> (Basis Z2StGen, Basis Z2StGen,Matrix ZMod2)
makeMatrix dta serreCartan s t_s = (dom,codom,induceMatrix dom codom  mp)
  where mp =  gensUpToMap dta s (t_s)
        dom = z2Basis serreCartan t_s mp 
        codom = z2Basis serreCartan (t_s+1) $ gensUpToMap dta (s-1) t_s
        
        
differential :: E2GenData -> (Z2FreeSteenrodVS) -> Z2FreeSteenrodVS
differential dta v = smap (\(Tensor sq x@(E2Gen s t_s _)) -> (toFModule $ Tensor sq unit)*(fromJust $ Map.lookup x $ (gensDiffMap dta)!(s,t_s))) v

isCycle dta v = 0 == differential dta v

-- it is assume that v is a cycle = boundry
-- we unfortunately need the Serre Cartan Basis for this
-- we removed the homogeniety restriction
antiDifferential :: (Array Int [SteenrodAlgebra]) -> E2GenData -> Z2FreeSteenrodVS -> Z2FreeSteenrodVS
antiDifferential scb dta 0 = 0
antiDifferential scb dta v = sum $ map (\((dom,cdom,mat),u) -> recompose dom $ preimage (decompose cdom u)  mat) parts 
  where parts = map (\((s,t_s),u) -> (makeMatrix dta scb (s+1) (t_s-1),u)) $ Map.toList $ Map.map sum pmap
        pmap = foldr (\u mp -> Map.insertWith (++) (biDeg u) [u] mp) Map.empty $ map (\(m,r) -> r *> (toFModule m)) $ toAList v
        




-- QAD debugging stuff

{-
loadE2Page 20
let dta  = it

let scb = admisArray 20
let s = 3 :: Int
let t_s = 17 :: Int
let  lmp =  gensUpToMap dta s (t_s-1)
let        cmp =  gensUpToMap dta (s-1) t_s
let        ldom = z2Basis scb t_s lmp 
let        cdom = z2Basis scb (t_s+1) cmp
let        rdom = z2Basis scb (t_s+2) $ gensUpToMap dta (s-2) t_s
let        lmatrix = induceMatrix ldom cdom lmp
let        cmatrix = induceMatrix cdom rdom cmp
let        (imag,_,rank,_) = imageKernel lmatrix
let        (_,kern,_,nullity) = imageKernel cmatrix
let        newPureVects = take (nullity-rank) $ filter (\v -> not $ v `vin` imag) kern
let        newImagVects = map (recompose cdom) newPureVects
-}