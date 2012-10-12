module SpectralLinear where

import SpectralDiffs
import SpectralRewrite
import qualified Data.Set as Set
import Recursive
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Utils
import EqClasses
import Data.Array.Unboxed
import ZMod2
import Data.Maybe
import Z2MatrixOps

identifyLinearTerms :: SpectralLogic -> Maybe (SpectralLogic,[(Bool,[SpectralLogic])])
identifyLinearTerms andNote@(SL _ (And st)) = do
  (t',lins) <- acSplit isLT andNote
  return (t',map doLT lins)
  where isLT x = (isXOr x) || ((isNot x) && (all isXOr $ getChildren x))
        doLT x
          | isNot x = (False,getChildren $ head $ getChildren x)
          | isXOr x = (True,getChildren x)
identifyLinearTerms _ = Nothing                      

enumerateTerms :: [SpectralLogic] -> Map.Map SpectralLogic Int
enumerateTerms logs = snd $ execState state (1,Map.empty)
  where state = mapM (\tm -> modify $ \(c,mp) -> (c+1,Map.insert tm c mp))
                $ sortNub logs

linearReduceTerm :: SpectralLogic -> Maybe (SpectralLogic,Map.Map BitVarTag SpectralLogic)
linearReduceTerm tm = do
  (ts,lns) <- identifyLinearTerms tm
  let mp = enumerateTerms $ concatMap snd lns
      matrices = linearIntEqsToMatrices $ map (\(b,sps) -> (b,Set.map (fromJust.(flip Map.lookup mp)) $ Set.fromList sps)) lns
      rrefmatrices = map (\(a,b) -> (a,rref b)) matrices
      ixMapArr = ixMapToArray mp
      xors = concatMap (\(eqc,mat) -> let ((a,b),(c,d)) = bounds mat 
                                      in map (\j -> 
                                               (mat!(c,j)==1,mapMaybe (\(i,i') -> maybeIf (mat!(i,j) == 1) (ixMapArr!i')) $ zip [a..c-1] (Set.toList eqc)))
                                         [b..d])
             rrefmatrices
      (hasbv,nohasbv) = partition (\(_,xortms) -> any isBitVar xortms) $ xors
      ts' = acInserts (map (\(b,xortms) -> (if b then id else slNot) $ slXOr xortms) nohasbv) ts
      bitvarmap = Map.fromList $ map (\(b,xortms) -> let (bvhead,xortms') = removeOne isBitVar xortms 
                                                     in (getTag bvhead,(if b then id else slNot) $ slXOr xortms')) hasbv
  return (ts',bitvarmap)
                                                        
linearIntEqsToMatrices :: [(Bool, Set.Set Int)] -> [(Set.Set Int,UArray (Int,Int) ZMod2)]
linearIntEqsToMatrices  linsint = zip classes matrices    
  where summands = map (\eqc -> (eqc,filter (\(_,a) -> Set.member (Set.findMin a) eqc) linsint)) classes
        classes = eqClasses $ map snd linsint
        matrices = map (\(eqc,ls) -> array ((1,1),(1+(Set.size eqc),length ls))
                                   $ concatMap (\(j,(b,s)) -> [((1+(Set.size eqc),j),ZMod2 b)] ++ [((i',j),ZMod2 $ Set.member i s) | (i,i') <- zip (Set.toList eqc) [1..Set.size eqc]])
                                   $ zip [1..] $ ls)
                 summands

