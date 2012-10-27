module JHomRecognizer where

import Data.List
import Data.Either
import Data.Array
import Control.Monad.Trans.Maybe
import Recursive
import SpectralDiffs
import SpectralRewrite
import SpectralLinear
import E2Saver
import E2Calculator
import E2Gen
import YonedaHtpyOps
import Utils
import Module
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import Z2LinearAlgebra
import Data.Maybe
import Debug.Trace
import Data.Ratio

bernoulli m = sum [((-1)^v) * (fromInteger $ choose (fromInteger k) (fromInteger v)) 
                   * ((v^m)%(k+1))
                   | k <- [0..m], v <- [0..k]]
              
orderImageJ 0 = 1
orderImageJ r
  | (r `mod` 8) `elem` [0,1] = 2
  | r `mod` 4 == 3           = componantOf 2 $ denominator $ ((bernoulli (2*n))/(fromIntegral $ 4*n))
  | otherwise                = 1
  where n = (r+1) `div` 4                          
                                         
adamsEdge t_s = head $ [s | s <- [3+(t_s`div` 2),2+(t_s`div` 2)..0], not $ beyondAdamsEdge s t_s]
            
beyondAdamsEdge s t_s = t_s < fs
  where fs = 2*s-eps
        eps = if (s `mod` 4) < 2 then 1 else s `mod` 4
                                             
                                                                                      
genOfImJ dta t_s 
  | (t_s `mod` 8) == 0 =  toFModule $ head $ gensAt dta (3+4*((t_s-8)`div`8),t_s)
  | (t_s `mod` 8) == 1 =  toFModule $ head $ gensAt dta (4+4*((t_s-9)`div`8),t_s)
  | (t_s `mod` 8) == 3 =  toFModule $ head $ gensAt dta (1+4*((t_s-3)`div`8),t_s)
  | (t_s `mod` 8) == 7 =  let n = (round $ logBase 2 $ fromIntegral $ orderImageJ $ fromIntegral t_s)  
                          in if n == 4 
                             then fromJust $ genMult dta (E2Gen 1 0 0) $ (head $ gensAt dta ((adamsEdge t_s) - n,t_s))
                             else toFModule $ head $ gensAt dta (1+(adamsEdge t_s) - n,t_s)

imageJConditions dta t_s = slAnd $ (concatMap (\k -> map (\r -> eqZero $ diff r $ proj r$ gensToST k) [2..ae-(e2PCGrating k)+2]) imj)
                           ++ (map (\k -> slOr $ map (\r -> slNot $ eqZero $ diff r $ proj r $ genToST k) [2..ae-(grating k)+2]) belowImJ)
                           ++ (map (\k -> slNot $ eqZero $ proj (e2PCGrating k) $ gensToST k) imj)
  where g = genOfImJ dta t_s
        ae = adamsEdge t_s
        imj = takeWhile (/=0) $ map (\i -> fromJust $ gensMult dta g $ toFModule $ E2Gen i 0 0) [0..]
        belowImJ = filter (\l -> (Just 0) /= (genMult dta l $  E2Gen 1 0 0)) $ gensAt dta ((e2PCGrating g)-1,t_s)
