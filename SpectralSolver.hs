module SpectralSolver where

import Data.List
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

-- this may end up being inefficient
toQuotient' :: [E2PageConst] -> E2Gen -> E2PageConst
toQuotient' [] x = toFModule x
toQuotient' (t:ts) x = if x ==  lastT 
                      then  initT
                      else toQuotient' (map (toQuotient'' lastT initT) ts) x
  where lastT = fst $ last $ toAList t
        initT = fromAList $ init $ toAList t
toQuotient'' lastT initT x = (smap (\y -> if y == lastT then initT else toFModule y)) x
toQuotient lst x = smap (\y -> toQuotient' lst y) x

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
      cycles <- specDiffKern (r-1) s t_s -- currently a stub, works only for orthogonal things
      pcycs <-  mapM (projectToPage r) cycles -- there is no way in hell this actually works.  
      result <- mapM specTermToE2PageConst $ nub pcycs
      return result

      
-- function neesds some work
specDiffKern :: Int -> Int -> Int -> SSState [SpectralTerm]
specDiffKern r s t_s = do
  basis <- perferredBasis (r-1) s t_s
  diffsBasis <- mapM ((spectralDifferentialExp (r-1)).gensToST) basis
  let (cycs,ncycs) = partition (\(b,db) -> db == 0) $ zip basis diffsBasis
  if 1 >= length ncycs then return $ map (gensToST.fst) cycs else error "cannot, at this time, compute kernel due to laziness of programmer"

      
specTermToE2PageConst :: SpectralTerm -> SSState E2PageConst
specTermToE2PageConst = nyi

projectToPage :: Int -> SpectralTerm -> SSState SpectralTerm
projectToPage 2 x = return x
projectToPage r x = do
  x' <- specTermToE2PageConst x
  let (s,t_s) = stBiDeg x
  -- technically no need to go to r-1, but no harm either in next line, might want to make this better
  quotDenoms <-  mapM (\r' -> perferredBasis r' (s+1) (t_s-r') >>= (mapM ((spectralDifferentialExp r').gensToST))) [2..r-1] 
  return $ gensToST $ toQuotient (nub $ filter (/=0) $ concat quotDenoms) x'
  
spectralDifferential :: Int -> SpectralTerm -> SSState SpectralTerm
spectralDifferential r tm = nyi

spectralDifferentialExp :: Int -> SpectralTerm -> SSState E2PageConst
spectralDifferentialExp r tm = nyi
