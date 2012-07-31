module E2Gen where

import Data.Char
import Data.List
import Data.Maybe
import Utils
import Text.Read
import Module
import SteenrodAlgebra
import Tensor
import ZMod2

data E2Gen = E2Gen Int Int Int deriving (Eq,Ord)

englishLetters :: [String]
englishLetters = map return ['A'..'Z']

letters :: [String]
letters = "i":(englishLetters ++ (concatMap (\ls -> map (ls++) englishLetters) letters))

degree (E2Gen _ x _) = x
grating (E2Gen x _ _) = x

instance Show E2Gen where
  show (E2Gen s t_s primes) = (letters!!s) ++ "_" ++ (texShow $ s + t_s) ++ (take primes $ repeat '\'')

instance UnShow E2Gen where
  unShow ss = E2Gen s ((read nm)-s) $ length $ takeWhile (=='\'') $ reverse ss
    where (name,num) = break (=='_') ss
          s =  (fromJust $ elemIndex name letters)
          nm = takeWhile isDigit  $ if num!!1 == '{' then drop 2 num else drop 1 num

instance Multiplicative E2Gen where
  unit = E2Gen 0 0 0

type FreeSteenrodModule = FreeModule E2Gen SteenrodAlgebra
type Z2FreeSteenrodVS = FreeModule (Tensor SteenrodSquare E2Gen) ZMod2

