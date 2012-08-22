{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SpectralRewrite where

import E2Saver
import E2Gen
import Control.Monad
import SpectralDiffs
import Recursive
import Data.Maybe
import Utils
import Data.List
import YonedaHtpyOps
import Data.Array
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import E2Calculator
import Module

type Rule a = a -> Maybe a

-- all that, and I have to do this with explicitness
normalize :: (Normable s, Normable r,MuRecursive s r, MuRecursive r s) =>
             [Rule r] -> [Rule s] -> r -> r
normalize fs gs x
  | isNorm x  = x
  | otherwise = case compactRules fs $ x' of
    Nothing -> setNorm True x'
    (Just x'') -> normalize fs gs x''
    where x' = muRecChildren x (map (normalize fs gs) rs, map (normalize gs fs) ss)
          (rs,ss) = muGetChildren x

  
unNormalize x = muMapBot (setNorm False) (setNorm False :: SpectralTerm -> SpectralTerm) x

compactRules :: [Rule a] -> Rule a
compactRules rs x = safeHead $ mapMaybe ($x) rs

upgradeRule :: (DataWrap dw dat) => Rule dat -> Rule dw
upgradeRule r wr = fmap (wrapData wr) $ r $ getData wr

acSplit :: (Recursive acop) => (acop -> Bool) -> acop -> Maybe (acop,[acop])
acSplit pred r = let (ots,res) = partition pred $ getChildren r in
  case ots of
    [] -> Nothing
    _  -> Just (recChildren r res,ots)

acInserts :: (Recursive acop) => [acop] -> acop -> acop
acInserts lst r = recChildren r $ lst ++ (getChildren r)

acMap :: (Recursive acop) => (acop -> acop) -> acop -> acop
acMap f r = recChildren r $ map f $ getChildren r

associate :: (Recursive r) => (r -> Bool) -> Rule r
associate pred s = do
  (s',insts) <- acSplit pred s
  return $ acInserts (concatMap getChildren insts) s'

distribute :: (Recursive r) => (r -> Bool) -> (r -> r -> r) -> Rule r
distribute pred prod s = do
  (s',dists) <- acSplit pred s
  let s'' = acInserts (tail dists) s'
      sm = head dists
  return $ acMap (prod s'') sm

-- should prolly do a real search
idempote :: (Recursive r, Eq r) => (r -> Bool) -> Rule r
idempote pred s = do
  (s',_) <- acSplit pred s
  return s'

-- should prolly do a real search
nilpote :: (Recursive r, Eq r) => (r -> Bool) -> r -> Rule r
nilpote pred x s = do
  guard $ any pred $ getChildren s
  return x

emptyNode x s = maybeIf (null $ getChildren s) x
singleNode s = case getChildren s of
  [x] -> Just x
  _ -> Nothing

opPushUp prdct pred t = do
  (t',projs) <- acSplit pred t
  guard $ length projs > 1
  return $ acInserts [recChildren (head projs) [prdct $ concatMap getChildren projs]] t'

-- (p (+ x y z)) -> (+ (p x) (p y) (p z))
opPushDown sumat pred t = do
  let [arg] = getChildren t
  guard $ pred arg
  let chlds = getChildren arg
  return $ sumat $ map (\s -> recChildren t [s]) chlds


----- now, some actual rules ------
easyZero :: ASSData -> Rule SpectralTerm
easyZero _ v
  | isZero v = Nothing
easyZero (ASSData gdt _ _)  v@(ST v' _) = maybeIf (or [s < 0, t_s < 0, t_s>0 && s > t_s, (inRange (bounds $ gensDiffMap gdt) (s,t_s)) && 0 == (Map.size $ (gensDiffMap gdt)!(s,t_s))])
                                          STZero
  where (s,t_s) = stBiDeg v

equalsZero (SL _(EqualZero z))
  | isZero z = Just 1
  | isDots z = Just 0
equalsZero _ = Nothing

dotsZero (ST _ (Dots 0)) = Just STZero
dotsZero _ = Nothing

easyCoef (ST v (WithCoef 0 x)) = Just 0
easyCoef (ST v (WithCoef 1 x)) = Just x
easyCoef (ST v (WithCoef _ z))
  | isZero z = Just 0
easyCoef _ = Nothing

easyNot (SL _ (Not (SL v (Not x)))) = Just x
easyNot (SL _ (Not LTrue)) = Just LFalse
easyNot (SL _ (Not LFalse)) = Just LTrue
easyNot _ = Nothing

xorNotRemove t = do
  (t',nots) <- acSplit isNot t
  let unnots = concatMap getChildren nots
  if odd $ length unnots
    then return $ slNot $ acInserts unnots t'
    else return $ acInserts unnots t'

coefExpand (ST v' (WithCoef xorn x))
  | isXOr xorn = Just $ sum $ map (\r -> (ST v' (WithCoef r x))) $ getChildren xorn
  | otherwise  = Nothing
mod2FoldMS ms = maybeIf (any (>1) $ map snd $ MS.toAscOccurList ms)
                     $   MS.fromAscOccurList $ filter ((>0).snd) $ map event $ MS.toAscOccurList ms
  where event (x,n) = if even n then (x,0) else (x,1)
mod2Fold (Plus ms) = fmap Plus $ mod2FoldMS ms
mod2FoldXor (XOr ms) = fmap XOr $ mod2FoldMS ms


easyDefined0 (SL _ (Tag Defined z)) = maybeIf (isZero z) 1
easyDefined0 _ = Nothing
expandDefinedProj (SL _ (Tag Defined (ST _ (Projection i x)))) = Just $ edc i
  where edc 2 = 1
        edc j = (eqZero (diff (j-1) $ proj (j-1) x)) &&& (edc (j-1))
expandDefinedProj _ = Nothing

prodConstFold assdt tms = do
  (t',consts) <- acSplit isDots tms
  guard $ not $ null $ tail consts
  prd <- foldM (\res g -> fmap sum $ mapM (\(h,_) -> genMult assdt g h) $ toAList res) (toFModule unit) $ concatMap (\(ST _ (Dots g)) -> map fst $ toAList g) consts :: Maybe E2PageConst
  return $ if prd == 0 then STZero  else acInserts [gensToST prd] t'

basicTermRule dta y = (compactRules $ (easyZero dta):rsl) y
  where rsl :: [Rule SpectralTerm]
        rsl = case y of
          (ST v (Plus _)) -> [associate isSum, idempote isZero,emptyNode STZero, opPushUp sum isDiff, opPushUp sum isProj, singleNode,upgradeRule mod2Fold]
          (ST v (Times _)) -> [ nilpote isZero STZero,associate isProd, idempote isOne, singleNode, emptyNode 1,
                               distribute isSum (*), opPushUp product isProj, prodConstFold dta]
          (ST v (Differential i x)) -> [nilpote isZero STZero]
          (ST v (Projection i x)) -> [nilpote isZero (STZero)]
          (ST v (SteenrodOp i x)) -> [opPushDown sum isSum,  nilpote isZero (STZero)]
          (ST v (WithCoef i x)) -> [easyCoef, opPushDown sum isSum, coefExpand]
          (ST v (Dots _)) -> [dotsZero]

basicLogicRule dta y = (compactRules rsl) y
  where rsl = case y of
          (SL v (And _)) -> [nilpote isFalse 0, associate isAnd, idempote isTrue, emptyNode 1, singleNode]
          (SL v (Or _)) -> [nilpote isTrue 1,associate isOr, idempote isFalse, emptyNode 0, singleNode]
          (SL v (Not _)) -> [opPushDown slOr isAnd, opPushDown slAnd isOr, easyNot]
          (SL v (EqualZero _)) -> [equalsZero]
          (SL v (XOr _)) -> [associate isXOr, idempote isFalse, emptyNode 0, singleNode, xorNotRemove,upgradeRule mod2FoldXor]
          (SL v (Tag Defined _)) -> [easyDefined0,expandDefinedProj]
          (SL v _) -> []

