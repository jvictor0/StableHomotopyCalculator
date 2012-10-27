{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SpectralDiffs where

import Recursive
import qualified Data.MultiSet as MS
import YonedaHtpyOps
import LispShow
import SteenrodAlgebra
import Utils
import E2Gen
import qualified Data.Set as Set
import Module
import Debug.Trace



data SpectralTerm = ST STAux STData | STZero  deriving (Eq,Ord)
data STAux = STA {staFilt :: Int, staDeg :: Int, staNorm :: Bool} deriving (Show, Eq, Ord)
data STData = Plus (MS.MultiSet SpectralTerm) |
              Times (MS.MultiSet SpectralTerm) |
              SteenrodOp Int SpectralTerm |
              Differential Int SpectralTerm |
              Projection Int SpectralTerm |
              WithCoef SpectralLogic SpectralTerm |
              Dots E2PageConst 
            deriving (Eq,Ord)

data SpectralLogic = SL SLAux SLData | LTrue | LFalse  deriving (Eq, Ord)
data SLAux = SLA {slaNorm :: Bool}  deriving (Show,Eq,Ord)
data SLData = EqualZero SpectralTerm |
              And (Set.Set SpectralLogic) |
              Or (Set.Set SpectralLogic) |
              XOr (MS.MultiSet SpectralLogic) |
              Not SpectralLogic |
              Tag SpectralTag SpectralTerm |
              BitVar BitVarTag
            deriving (Eq,Ord)

data SpectralTag = Defined | PermCycle deriving (Eq,Ord)
data BitVarTag = DiffCoef Int E2PageConst E2PageConst deriving (Eq,Ord)

instance Show SpectralTag where
  show Defined = "defined?"
  show PermCycle = "perm-cycle?"

instance Show SpectralLogic where
  show (SL _ d) = show d
  show LTrue = "True"
  show LFalse = "False"
instance Show SpectralTerm where
  show (ST _ d) = show d
  show STZero = "0" 

instance Show STData where
  show (Plus st) = lispShow "+" $ map show $ MS.toList st
  show (Times st) = lispShow "*" $ map show $ MS.toList st
  show (SteenrodOp n l) = lispShow (show $ Sq[n]) [show l]
  show (Differential n l) = lispShow ("d_" ++ (texShow n)) [show l]
  show (Projection n l) = lispShow ("p_" ++ (texShow n)) [show l]
  show (WithCoef l t) = lispShow "*>" [show l,show t]
  show (Dots d) = show d
  
instance Show SLData where
  show (Tag t tm) = lispShow (show t) $ [show tm]
  show (Not tm) = lispShow "not" [show tm]
  show (XOr ms) = lispShow "xor" $ map show $ MS.toList ms
  show (And ms) = lispShow "and" $ map show $ Set.toList ms
  show (Or ms) = lispShow "or" $ map show $ Set.toList ms
  show (EqualZero a) = lispShow "zero?"$ map show [a]
  show (BitVar v) = show v

instance Show BitVarTag where
  show (DiffCoef i targ src) = "d_" ++ (texShow i) ++ "^{" ++ (show targ) ++ "}(" ++ (show src) ++ ")"

class Normable hb where
  isNorm :: hb -> Bool
  setNorm :: Bool -> hb -> hb

instance Normable SpectralTerm where
  isNorm STZero = True
  isNorm (ST v _) = staNorm v
  setNorm b x = stSetNorm b x
instance Normable SpectralLogic where
  isNorm LTrue = True
  isNorm LFalse = True
  isNorm (SL v _) = slaNorm v
  setNorm b x = slSetNorm b x

class DataWrap dw dat where
  getData :: dw -> dat
  wrapData :: dw -> dat -> dw

instance DataWrap SpectralTerm STData  where
  getData (ST _ x) = x
  getData _ = error "Cannot get Data of Zero"
  wrapData (ST v _) x = setNorm False $ ST v x
  wrapData x y = x
instance DataWrap SpectralLogic SLData where
  getData (SL _ x) = x
  getData _ = error "Cannot get Data of True or False"
  wrapData (SL v _) x = setNorm False $ SL v x
  wrapData x y = x
  

instance Recursive SpectralTerm where
  getChildren r = fst (muGetChildren r :: ([SpectralTerm],[SpectralLogic]))
  recChildren STZero [] = STZero
  recChildren v x
   | isNorm v = recChildren (setNorm False v) x
  recChildren (ST v (WithCoef i _)) [t] = ST v (WithCoef i t)
  recChildren r rs = muRecChildren r (rs,[]::[SpectralLogic])

instance Recursive SpectralLogic where
  getChildren r = fst (muGetChildren r :: ([SpectralLogic],[SpectralTerm]))
  recChildren LFalse [] = 0
  recChildren LTrue [] = 1
  recChildren v x
   | isNorm v = recChildren (setNorm False v) x
  recChildren (SL v (EqualZero i)) [] = SL v (EqualZero i)
  recChildren (SL v (Tag tg t)) [] = SL v (Tag tg t)
  recChildren r rs = muRecChildren r (rs,[]::[SpectralTerm])
  
instance MuRecursive SpectralTerm SpectralLogic where
  muGetChildren (ST _ (Plus ms)) = (MS.toList ms,[])
  muGetChildren (ST _ (Times ms)) = (MS.toList ms,[])
  muGetChildren (ST _ (SteenrodOp i t)) = ([t],[])
  muGetChildren (ST _ (Differential i t)) = ([t],[])
  muGetChildren (ST _ (Projection i t)) = ([t],[])
  muGetChildren (ST _ (WithCoef s t)) = ([t],[s])
  muGetChildren _ = ([],[])


  muRecChildren STZero ([],[]) = 0
  muRecChildren v x
   | isNorm v = muRecChildren (setNorm False v) x
  muRecChildren (ST v (Plus ms)) (ls,[]) = ST v $ Plus $ MS.fromList ls
  muRecChildren (ST v (Times ms)) (ls,[]) = ST v $ Times $ MS.fromList ls
  muRecChildren (ST v (Differential i _)) ([t],[]) = ST v (Differential i t)
  muRecChildren (ST v (SteenrodOp i _)) ([t],[]) = ST v (SteenrodOp i t)
  muRecChildren (ST v (Projection i _)) ([t],[]) = ST v (Projection i t)
  muRecChildren (ST v (WithCoef _ _)) ([t],[i]) = (ST v (WithCoef i t))
  muRecChildren r ([],[]) = r


instance MuRecursive SpectralLogic SpectralTerm where
  muGetChildren (SL _ (And ms)) = (Set.toList ms,[])
  muGetChildren (SL _ (Or ms)) = (Set.toList ms,[])
  muGetChildren (SL _ (XOr ms)) = (MS.toList ms,[])
  muGetChildren (SL _ (Not t)) = ([t],[])
  muGetChildren (SL _ (EqualZero t)) = ([],[t])
  muGetChildren (SL _ (Tag _ t)) = ([],[t])
  muGetChildren _ = ([],[])

  muRecChildren LFalse ([],[]) = LFalse
  muRecChildren LTrue ([],[]) = LTrue
  muRecChildren v x
   | isNorm v = muRecChildren (setNorm False v) x
  muRecChildren (SL v (And ms)) (ls,[]) = (SL v $ And $ Set.fromList ls)
  muRecChildren (SL v (Or ms)) (ls,[]) = SL v $ Or $ Set.fromList ls
  muRecChildren (SL v (XOr ms)) (ks,[]) = SL v $ XOr$ MS.fromList ks
  muRecChildren (SL v (Not _)) ([t],[]) = SL v $ Not t
  muRecChildren (SL v (EqualZero _)) ([],[t]) = SL v $ EqualZero t
  muRecChildren (SL v (Tag tg _))  ([],[t]) = SL v $ Tag tg t
  muRecChildren r ([],[]) = r


stBiDeg (ST v _) = (staDeg v,staFilt v)
stIsNorm (ST v _) = staNorm v
stIsNorm _ = True
stSetNorm b (ST v d) = ST (v{staNorm = b}) d
stSetNorm _ x = x
biDegToAux (x,y) = STA {staFilt = y, staDeg = x, staNorm = False}
auxSum v v' = let ((x,y),(z,w)) = (stBiDeg v,stBiDeg v') in biDegToAux (x+z,y+w)

slSetNorm b (SL v d) = SL (v{slaNorm = b}) d
slSetNorm b x = x
slauxDefault = SLA {slaNorm = False}

isSum (ST _ (Plus _)) = True
isSum _ = False
isProd (ST _ (Times _)) = True
isProd _ = False
isDiff (ST _ (Differential _ _)) = True
isDiff _ = False
isProj (ST _ (Projection _ _)) = True
isProj _ = False
isSTO (ST _ (SteenrodOp _ _)) = True
isSTO _ = False
isWCo (ST _ (WithCoef _ _)) = True
isWCo _ = False
isDots (ST _ (Dots _)) = True
isDots _ = False
isZero STZero = True
isZero (ST _ (Dots 0)) = True
isZero _ = False
isOne (ST _ (Dots d)) = (d == toFModule unit)
isOne v =  ((isProj v) && (isOne $ (\x -> trace (show x) $ head x) $ getChildren v))


isAnd (SL _ (And _)) = True
isAnd _ = False
isOr (SL _ (Or _)) = True
isOr _ = False
isXOr (SL _ (XOr _)) = True
isXOr _ = False
isNot (SL _ (Not _)) = True
isNot _ = False
isEq (SL _ (EqualZero  _)) = True
isEq _ = False
isTag (SL _ (Tag _ _)) = True
isTag _ = False
isTrue = (LTrue ==)
isFalse = (LFalse ==)
isBitVar (SL _ (BitVar _)) = True
isBitVar _ = False

getTag (SL _ (BitVar x)) = x
getTag x = error $ "wtf: " ++ (show x)
getDots (ST _ (Dots x)) = x

linearTerm (ST _ (WithCoef _ a@(ST _ (Dots _)))) = Just a
linearTerm a@(ST _ (Dots _)) = Just a
linearTerm _ = Nothing


diffCoef r src trg = ST (biDegToAux (a,b)) 
                   (WithCoef (SL slauxDefault (BitVar (DiffCoef r trg src)))
                    (gensToST trg))
  where (E2Gen a b c) = fst $ (\x -> trace (show x) $ head x) $ toAList trg
        

instance Module SpectralTerm SpectralLogic where
  log *> tm = ST (biDegToAux $ stBiDeg tm) $ WithCoef log tm
                   
instance Num SpectralLogic where
  d@(SL v _) + d'@(SL v' _) = slSetNorm False $ SL v $ XOr $ MS.fromList [d,d']
  LFalse + x = x
  LTrue + x = slNot x
  x + y = y + x
  d@(SL v _) * d'@(SL v' _) = slSetNorm False $ SL v $ And $ Set.fromList [d,d']
  LFalse * x = LFalse
  LTrue * x = x
  x * y = y * x
  (-) = dumbError
  fromInteger 0 = LFalse
  fromInteger 1 = LTrue
  fromInteger n = dumbError
  signum = dumbError
  abs = dumbError

instance Num SpectralTerm where
  STZero + x = x
  x + STZero = x
  d@(ST v _) + d'@(ST v' _) = stSetNorm False $ ST v $ Plus $ MS.fromList [d,d']
  STZero * x = STZero
  x * STZero = STZero
  d * d' = ST (auxSum d d') $ Times $ MS.fromList [d,d']
  (-) = (+)
  fromInteger 1 = ST (biDegToAux (0,0)) $ Dots $ toFModule $ E2Gen 0 0 0
  fromInteger 0 = STZero
  fromInteger _ = dumbError
  signum = dumbError
  abs = dumbError

diff i t = ST (biDegToAux (s+i,t_s-1)) $ Differential i t
  where (s,t_s) = stBiDeg t
proj i t = ST (biDegToAux (s,t_s)) $ Projection i t
  where (s,t_s) = stBiDeg t

genToST g@(E2Gen a b c) = ST (biDegToAux (a,b)) $ Dots $ toFModule g
gensToST 0 = STZero
gensToST gs = ST (biDegToAux (a,b)) $ Dots gs
  where (E2Gen a b c) = fst $ (\x -> trace (show x) $ head x) $ toAList gs

(===) :: SpectralTerm -> SpectralTerm -> SpectralLogic
a === b = SL slauxDefault (EqualZero (a - b))
eqZero a = SL slauxDefault (EqualZero a)
(&&&) :: SpectralLogic -> SpectralLogic -> SpectralLogic
a &&& b = a * b
(|||) :: SpectralLogic -> SpectralLogic -> SpectralLogic
a ||| b = SL slauxDefault (Or $ Set.fromList [a,b])
slNot a = SL slauxDefault $ Not a
slAnd lst = SL slauxDefault $ And $ Set.fromList lst
slOr lst = SL slauxDefault $ Or $ Set.fromList lst
slXOr lst = SL slauxDefault $ XOr $ MS.fromList lst



stDefined i a = slAnd $ map (\j -> eqZero $ diff j $ proj j a) [2..i-1]

