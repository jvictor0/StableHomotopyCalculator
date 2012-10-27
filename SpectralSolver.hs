module SpectralSolver where

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
import JHomRecognizer
import System.IO.Unsafe

-- this may end up being inefficient
toQuotient' :: [E2PageConst] -> E2Gen -> E2PageConst
toQuotient' [] x = toFModule x
toQuotient' (t:ts) x = if x ==  lastT 
                       then toQuotient (map (toQuotient'' lastT initT) ts) initT
                       else toQuotient' (map (toQuotient'' lastT initT) ts) x
  where lastT = fst $ last $ toAList t
        initT = fromAList $ init $ toAList t
        toQuotient'' lastT initT x = (smap (\y -> if y == lastT then initT else toFModule y)) x
toQuotient :: [E2PageConst] -> E2PageConst -> E2PageConst
toQuotient lst x = smap (toQuotient' lst) x

type SSMTState a = MaybeT (State SSStateData) a
type SSState a = State SSStateData a
data SSStateData = SSSD {sssASSData :: ASSData,
                         sssDiffMap :: Map.Map (Int,E2PageConst) SpectralTerm,
                         sssPerfBasis :: Map.Map (Int,Int,Int) (Maybe [E2PageConst]),
                         sssKnownBitVars :: Map.Map BitVarTag SpectralLogic} deriving (Eq,Show)


type Rule a = a -> SSState (Maybe a)

preRuleToRule :: PreRule a -> Rule a
preRuleToRule r a = return $ r a

basicTermRule dta = preRuleToRule $ basicTermPreRule dta
basicLogicRule dta = preRuleToRule $ basicLogicPreRule dta

------------------Normalizer--------------
-- all that, and I have to do this with explicitness

-- todo: make it able to break early when doing children and apply specified rules before worrying about children

normalize :: (Normable s, Normable r,MuRecursive s r, MuRecursive r s, 
              Show s, Show r, Num r, Num s, Eq r, Eq s) =>
             ([Rule r],[Rule r]) -> ([Rule s],[Rule s]) 
             -> (r -> r -> Maybe r) -> (s -> s -> Maybe s) -> r -> SSState r
normalize fs gs fhalts ghalts x
  | isNorm x  = return x
  | otherwise = do
    preResult <- compactRules (snd fs) x
    case preResult of
      (Just result) -> normalize fs gs fhalts ghalts result
      Nothing -> do
        let (rs,ss) = muGetChildren x
        rs'' <- mapHaltM (fhalts x) (normalize fs gs fhalts ghalts) rs
        case rs'' of
          (Right result) -> normalize fs gs fhalts ghalts result
          (Left rs') -> do
            ss' <- mapM (normalize gs fs ghalts fhalts) ss
            let x' =  muRecChildren x (rs',ss')
            result <- compactRules (fst fs) x'
            case result of 
              Nothing -> return $ setNorm True x'
              (Just x'') -> normalize fs gs fhalts ghalts x''

  
unNormalizeLogic x = muMapBot (setNorm False :: SpectralLogic -> SpectralLogic) (setNorm False :: SpectralTerm -> SpectralTerm) x
unNormalizeTerm x = muMapBot (setNorm False :: SpectralTerm -> SpectralTerm) (setNorm False :: SpectralLogic -> SpectralLogic) x


compactRules :: [Rule a] -> Rule a
compactRules rs x = firstJustM ($x) rs




-------------------Data organization code-------------------

getGensAt :: Int -> Int -> SSMTState [E2Gen]
getGensAt s t_s =  do
   fmap ((flip gensAt (s,t_s)).sssASSData) get


perferredBasis :: Int -> Int -> Int -> SSMTState [E2PageConst]
perferredBasis 2 s t_s =  fmap (map toFModule) $ getGensAt s t_s 
perferredBasis r s t_s =  do
  old_res <- fmap ((Map.lookup (r,s,t_s)) . sssPerfBasis) get
  case old_res of
    (Just (Just result)) -> return result
    (Just Nothing) -> fail ""
    _ -> do
      modify $ \g -> g{sssPerfBasis = Map.insert (r,s,t_s) Nothing (sssPerfBasis g)}
      cycles <- specDiffKern (r-1) s t_s -- currently a stub, works only if the number of noncycles is 1 or 0
      result <- fmap nub $ mapM (projectToPage r) cycles -- this has the bug of <x,y,z>/(x+y+z.)  
      modify $ \g -> g{sssPerfBasis = Map.insert (r,s,t_s) (Just result) (sssPerfBasis g)}
      return result

      
-- function neesds some work
specDiffKern :: Int -> Int -> Int -> SSMTState [SpectralTerm]
specDiffKern r s t_s =  do
  basis <- perferredBasis r s t_s
  diffsBasis <- mapM ((spectralDifferentialExp r).gensToST) basis
  let (cycs,ncycs) = partition (\(b,db) -> db == 0) $ zip basis diffsBasis
  if 1 >= length ncycs then return $ map (gensToST.fst) cycs else error "cannot, at this time, compute kernel due to laziness of programmer"

      
specTermToE2PageConst :: SpectralTerm -> SSMTState E2PageConst
specTermToE2PageConst (ST _ (Dots x)) = return x
specTermToE2PageConst x
  | isZero x = return 0 
  | isSum  x  =  fmap sum $ mapM specTermToE2PageConst $ getChildren x
  | otherwise = fail ""

projectToPage :: Int -> SpectralTerm -> SSMTState E2PageConst
projectToPage 2 x =  specTermToE2PageConst x
projectToPage r x =  do
  x' <- specTermToE2PageConst x
  let (s,t_s) = stBiDeg x
  -- technically no need to go to r-1, but no harm either in next line, might want to make this better
  quotDenoms <-  mapM (\r' -> perferredBasis r' (s-r') (t_s+1) >>= (mapM ((spectralDifferentialExp r').gensToST))) [2..min (r-1) s] 
  return $  toQuotient (nub $ filter (/=0) $ concat quotDenoms) x' 
  
  
-- definately wants a perferred basis element
spectralDifferential :: Int -> SpectralTerm -> SSMTState SpectralTerm
spectralDifferential r tm = do
  mp <- fmap sssDiffMap get
  tm' <- specTermToE2PageConst tm
  case Map.lookup (r,tm') mp of
    (Just result) -> return result
    Nothing -> do
      let (s,t_s) =  stBiDeg tm
      cbat <- perferredBasis r (s+r) (t_s-1) 
      let result =  sum $ map (\ctm -> diffCoef r tm' ctm) cbat  
      modify $ \old -> old{sssDiffMap = Map.insert (r,tm') result mp}
      return result
      
spectralDifferentialExp :: Int -> SpectralTerm -> SSMTState E2PageConst
spectralDifferentialExp r tm = (spectralDifferential r tm) >>= specTermToE2PageConst
                               

------------------ grab solved things, declare them solved ---------
detectSolved :: SpectralLogic -> SSMTState SpectralLogic
detectSolved andnode
  | isAnd andnode = do
    (ts,solved) <- MaybeT $ return $ acSplit (\t -> (isBitVar t) || ((isNot t) && (all isBitVar $ getChildren t))) andnode
    knowns <- fmap sssKnownBitVars get
    let (fls,trs) = partition isNot $ solved
    modify $ \state -> state{sssKnownBitVars =  Map.unions $ 
                                                [knowns,Map.fromList $ map (flip (,) LTrue) $ map getTag trs,
                                                 Map.fromList $ map (flip (,) LFalse) $ map (getTag.(\x -> head x).getChildren) fls]}
    return ts



-------------------rules using this --------------

doDifferential :: Rule SpectralTerm
doDifferential (ST _ (Differential r dot@(ST _ (Dots d)))) =  trace ("doing differential at " ++ (show (r,d)))$  runMaybeT $ do
  let dot' =  dot
  let (s,t_s) = stBiDeg dot'
  perfBasis <- perferredBasis r s t_s
  if null perfBasis
    then do
    dta <- get
    b16 <- projectToPage r dot
    error $ concat 
        [show b16,"\n\n",concat $ intersperse "\n" $ map show $ Map.toList $ sssDiffMap dta,"\n\n",
         concat $ intersperse "\n" $ map show $ Map.toList $sssPerfBasis dta,"\n\n",
         concat $ intersperse "\n" $ map show $ Map.toList $sssKnownBitVars dta]
    else return ()
  result <- fmap sum $ mapM ((spectralDifferential r).gensToST) $ filter (/=0) $ decomposeBasis d perfBasis
  return $  result
doDifferential _ = return Nothing


doProjection :: Rule SpectralTerm 
doProjection (ST _ (Projection r dot)) = do
  result <- runMaybeT $ fmap gensToST $ projectToPage r dot
  return $ result
doProjection _ = return Nothing

doProjectionLite term@(ST v (Projection 2 stuff)) = return stuff
doProjectionLite term@(ST v (Projection r stuff)) = do
  proj_m1 <- doProjectionLite (ST v (Projection (r-1) stuff))
  let (s,t_s) = stBiDeg proj_m1
  if r > s then return proj_m1 else do  
    bs <- perferredBasis r s t_s
    denom <- mapM (spectralDifferential r) $ map gensToST bs
    let result = slQuotient denom proj_m1
    MaybeT $ return result
doProjectionLite _ = MaybeT $ return Nothing


substituteBit :: Rule SpectralLogic
substituteBit (SL _ (BitVar b)) = do
  mp <- fmap sssKnownBitVars get
  return $ Map.lookup b mp 
substituteBit _ = return Nothing


------------------the actual solver----------------------

leibnizRule :: Int -> SpectralTerm -> SpectralTerm  -> SpectralLogic
leibnizRule i a b = (slNot $ stDefined i a) ||| (slNot $ stDefined i b) 
  ||| ((diff i $ proj i $ a * b) === ((diff i $ proj i a)*b + a*(diff i $ proj i b)))

leibnizRuleGen :: Int -> E2Gen -> E2Gen -> SpectralLogic
leibnizRuleGen i g h = leibnizRule i (genToST g) (genToST h)


-- want to say that h1 is a permanant cycle
specialDiffs (ST v (Differential _ (ST _ (Dots d)))) = maybeIf (d==(toFModule $ E2Gen 1 1 0)) 0
specialDiffs (ST v (Differential _ (ST _ (Projection _ (ST _ (Dots d)))))) = maybeIf (d==(toFModule $ E2Gen 1 1 0)) 0
specialDiffs _ = Nothing

startingTerm ::  ASSData -> SpectralLogic
startingTerm dta@(ASSData d _ _) = let ld = largestDegree d in
  slAnd $ [leibnizRuleGen i g h | g <- knownGens d, h <- knownGens d, g <= h, ld -1> (degree g) + (degree h), 
           ld -1> (grating g) + (grating h), i <- [2..ld-(grating g)-(grating h)]] 
           ++ [imageJConditions dta 15]
         

logicRegRules dta = [substituteBit,basicLogicRule dta]
termRegRules dta = [preRuleToRule specialDiffs, basicTermRule dta, doDifferential, doProjection]--,runMaybeT.doProjectionLite]
termPreRules dta = [preRuleToRule $ easyZero dta]
logicPreRules dta = []
logicRules dta = (logicRegRules dta,logicPreRules dta)
termRules dta = (termRegRules dta,termPreRules dta)

simplifyLogic term = (fmap sssASSData get) >>= 
                \dta -> normalize (logicRules dta) (termRules dta) logicHalt termHalt
                        $ unNormalizeLogic term
simplifyTerm term = (fmap sssASSData get) >>= 
                \dta -> normalize (termRules dta) (logicRules dta) termHalt logicHalt
                        $ unNormalizeTerm term

recognizeSolvedTerms term = runMaybeT $ detectSolved term

updateState = do
  diffMap <- fmap sssDiffMap get 
  diffMap' <- fmap Map.fromList $ mapM (\(k,val) -> fmap ((,) k) (simplifyTerm val)) $ Map.toList diffMap
  modify $ \g -> g{sssDiffMap = diffMap', sssPerfBasis = Map.filter isJust $ sssPerfBasis g}
  
  tagMap <- fmap sssKnownBitVars get 
  tagMap' <- fmap Map.fromList $ mapM (\(k,val) -> fmap ((,) k) (simplifyLogic val)) $ Map.toList tagMap
  modify $ \g -> g{sssDiffMap = diffMap'}
  
linearSolving :: SpectralLogic -> SSState (Maybe SpectralLogic)
linearSolving term = trace "linearSoling" $ runMaybeT $ do
  (newterm,addsMap) <- MaybeT $ return $ linearReduceTerm term
  modify $ \g -> g{sssKnownBitVars = Map.union addsMap (sssKnownBitVars g)}
  return newterm

runASSSolver :: SpectralLogic -> SSState SpectralLogic
runASSSolver term = do
  term' <-  trace "simplify" $ simplifyLogic term
  termsolve <- trace (take 30 $ show term') $ recognizeSolvedTerms term'
  case termsolve of
    (Just result) -> trace "restart" $ updateState >> (runASSSolver result)
    Nothing -> trace "linearsolving" $ do
      linearSolve <-  updateState >> linearSolving term'
      updateState
      case linearSolve of
        (Just result) -> runASSSolver result
        Nothing -> return term'

startingState dta = SSSD {sssASSData = dta,
                          sssDiffMap = Map.empty,
                          sssPerfBasis = Map.empty,
                          sssKnownBitVars = Map.empty}
endingState sd = (sssDiffMap sd,sssPerfBasis sd, sssKnownBitVars sd)


adamsSpectralSequenceSolve :: ASSData -> (SpectralLogic,SSStateData)
adamsSpectralSequenceSolve dta = runState (runASSSolver (startingTerm dta))$ 
                                 SSSD {sssASSData = dta,
                                       sssDiffMap = Map.empty,
                                       sssPerfBasis = Map.empty,
                                       sssKnownBitVars = Map.empty}

problemTerm dta = startingTerm dta
solveProblem = fmap (\dta -> evalState (simplifyLogic $ problemTerm $ makeASSData dta) $ startingState $ makeASSData dta) (loadE2Page 20)

doit = fmap ((\(f,s) -> (endingState s,f)).adamsSpectralSequenceSolve.makeASSData) (loadE2Page 18) 

