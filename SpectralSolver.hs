module SpectralSolver where

import Data.Either
import Data.Array
import Control.Monad.Trans.Maybe
import SpectralDiffs
import SpectralRewrite
import E2Saver
import E2Calculator
import E2Gen
import YonedaHtpyOps
import Utils
import Module
import Control.Monad.State
import qualified Data.Map as Map

-- want to say that h1 is a permanant cycle
specialDiffs (ST v (Differential _ (ST _ (Projection _ (ST _ (Dots d)))))) = maybeIf (d==(toFModule $ E2Gen 1 1 0)) 0
specialDiffs _ = Nothing

startingTerm ::  ASSData -> SpectralLogic
startingTerm dta@(ASSData d _ _) = let ld = largestDegree d in
  slAnd [leibnizRuleGen i g h | g <- knownGens d, h <- knownGens d, g <= h, ld -1> (degree g) + (degree h), ld -1> (grating g) + (grating h), i <- [2..ld-(grating g)-(grating h)]]

doit = fmap (\d -> normalize [basicLogicRule d] ([specialDiffs, basicTermRule d]) $ startingTerm d) $ fmap makeASSData $ loadE2Page 20

normIt x = fmap (\d -> normalize [basicLogicRule d] ([specialDiffs, basicTermRule d]) $ x) $ fmap makeASSData $ loadE2Page 1
normItT x = fmap (\d -> normalize ([specialDiffs, basicTermRule d]) [basicLogicRule d] $ x) $ fmap makeASSData $ loadE2Page 10


leibnizRule :: Int -> SpectralTerm -> SpectralTerm -> SpectralLogic
leibnizRule i a b = (slNot $ stDefined a) ||| (slNot $ stDefined b) ||| (slNot $ stDefined $ a*b)
  ||| ((diff i $ a * b) === ((diff i a)*b + a*(diff i b)))

leibnizRuleGen :: Int -> E2Gen -> E2Gen -> SpectralLogic
leibnizRuleGen i g h = leibnizRule i (proj i $ genToST g) (proj i $ genToST h)


type SSState a = MaybeT (State SSStateData) a
data SSStateData = SSSD {sssASSData :: ASSData,
                         sssDiffMap :: Map.Map (Int,E2PageConst) SpectralTerm,
                         sssPerfBasis :: Map.Map (Int,Int,Int) (Either [E2PageConst] Int),
                         sssCounter :: Int}

getGensAt :: Int -> Int -> SSState [E2Gen]
getGensAt s t_s = do
  (ASSData d _ _) <- fmap sssASSData get
  return $ map (fst) $ Map.toList $ (gensDiffMap d)!(s,t_s)

perferredBasis :: Int -> Int -> Int -> SSState [E2PageConst]
perferredBasis 2 s t_s = fmap (map toFModule) $ getGensAt s t_s 
perferredBasis r s t_s = do
  old_res <- fmap ((Map.lookup (r,s,t_s)) . sssPerfBasis) get
  j <- fmap sssCounter get
  case old_res of
    (Just (Left result)) -> return result
    (Just (Right i)) | i == j -> fail ""
    _ -> do
      modify $ \g -> g{sssPerfBasis = Map.insert (r,s,t_s) (Right j) (sssPerfBasis g)}
      result <- do
        r_1Basis <- perferredBasis (r-1) s t_s
        r_1BDiffs <- mapM ((spectralDifferentialExp (r-1)).gensToST) r_1Basis
        if all isZero r_1BDiffs
          then do
          return r_1Basis
          else if 1 == length r_1Basis
               then do
                 return []
               else error $ "Cannot compute perferred basis in this case because of laziness in E("
                    ++ (show r) ++ "," ++ (show s) ++ "," ++ show (t_s) ++ ")"
      modify $ \g -> g{sssPerfBasis = Map.insert (r,s,t_s) (Left result) (sssPerfBasis g)}
      return result


projectToPage :: Int -> SpectralTerm -> SSState SpectralTerm
projectToPage 2 x = return x
projectToPage r x = nyi
                                 

spectralDifferential :: Int -> SpectralTerm -> SSState SpectralTerm
spectralDifferential r tm = nyi

spectralDifferentialExp :: Int -> SpectralTerm -> SSState SpectralTerm
spectralDifferentialExp r tm = nyi
