{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SteenrodAlgebra where

import Module
import Utils
import ZMod2
import Data.Array
import Data.List
import Data.Char


data SteenrodSquare = Sq [Int] deriving (Eq, Ord)

instance Show SteenrodSquare where
  show (Sq []) = "1"
  show (Sq [x]) = "Sq^" ++ (texShow x)
  show (Sq ls) = "Sq^{\\{" ++ (cim "," show ls ) ++ "\\}}"

instance UnShow SteenrodSquare where
  unShow "1" = Sq[]
  unShow str = if take (length "Sq^{\\{") str == "Sq^{\\{"
               then Sq $ read $ "[" ++ (dropLast (length "\\}}") $ drop (length "Sq^{\\{") str) ++ "]"
               else if isDigit $ last str
                    then Sq [read [last str]]
                    else Sq [read $ (init $ drop (length "Sq^{") str)]

type SteenrodAlgebra = FreeModule SteenrodSquare ZMod2

instance Multiplicative SteenrodSquare where
  unit = Sq []
  (Sq x) <**> (Sq y) = decompAdmis $ Sq $ x ++ y

adem :: (Eq r,Num r) => Int -> Int -> FreeModule SteenrodSquare r
adem i j = fromAList [(Sq $ filter (/=0) [i+j-k,k],
                       fromInteger $ choose (j-k-1) (i-2*k))
                     | k <- [0..i`div`2]]

decompAdmis :: (Eq r,Num r) => SteenrodSquare -> FreeModule SteenrodSquare r
decompAdmis (Sq ss) = case admiss [] ss of
  Nothing -> toFModule $ Sq ss
  (Just (ad,i,j,rst)) -> (toFModule $ Sq ad)*(adem i j)*(decompAdmis $ Sq rst)
  where admiss _ [] = Nothing
        admiss _ [x] = Nothing
        admiss ps (x:y:rst)
          | x < 2*y   = Just (reverse ps,x,y,rst)
          | otherwise = admiss (x:ps) $ y:rst

admisArray :: Int -> Array Int [SteenrodAlgebra]
admisArray largeDeg = array (0,largeDeg) $ (0,[toFModule $ Sq []]):
                      [(i,map (toFModule.Sq) $ concatMap (\j -> aft j i) [1..i]) | i <- [1..largeDeg]]
  where aft i j
          | i == j     = [[i]]
          | i > j      = []
          | otherwise  = map (++[i]) $ concatMap (\k -> (aft k (j-i))) [2*i..j]

sq :: [Int] -> SteenrodAlgebra
sq ns = decompAdmis $ Sq ns
