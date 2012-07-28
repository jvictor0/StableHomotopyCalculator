{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
module MatrixOps where

import Data.Array.Unboxed
import Utils
import Data.Array.ST hiding (unsafeFreeze)
import Control.Monad
import Control.Monad.ST
import LinearAlgebra
import ZMod2
import Data.STRef
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array.Unsafe
import Data.List
import Data.Maybe

addColumn ::  (STUArray s (Int,Int) Bool) -> ((Int,Int),(Int,Int)) -> ST s ()
addColumn arr ((a,c),(source,target)) = do
  forM_ [a..c] $ \i -> do
    src <-  readArray arr (i,source)
    targ <- readArray arr (i,target)
    writeArray arr (i,target) (src `xor` targ)
  return ()


rrefWPivs :: Matrix ZMod2 -> (Matrix ZMod2,Map.Map Int Int, Map.Map Int Int)
rrefWPivs arr' =   runST $ do 
  let ((a,b),(c,d)) = bounds arr'
  pivs <- newSTRef $ (Map.empty,Map.empty)
  arr <- thaw $ amap (==1) arr' :: ST s (STUArray s (Int,Int) Bool)
  forM_ [a..c] $ \i -> do
    (to,from) <- readSTRef pivs
    fnd <-  findM (\j -> fmap (&&(not $ Map.member j from)) (readArray arr (i,j)) ) [b..d] 
    case fnd  of
      Nothing -> return ()
      (Just j) -> do
        modifySTRef pivs $ \(to,from) ->  (Map.insert i j to, Map.insert j i from)
        forM_ ([b..d]\\[j]) $ \k -> do
        ifM (readArray arr (i,k))
            (addColumn arr ((a,c),(j,k)))
            (return ())
  result <- unsafeFreeze arr
  (to,from) <- readSTRef pivs
  return (amap ZMod2 result,to,from)

rref :: Matrix ZMod2 -> Matrix ZMod2
rref arr = let (a,_,_) = rrefWPivs arr in a

imageKernel :: Matrix ZMod2 -> ([Vector ZMod2],[Vector ZMod2],Int,Int)
imageKernel mat' = (imag,kern,Map.size pto, c-a+1-(Map.size pto))
  where (mat,pto,pfrom) = rrefWPivs mat'
        ((a,b),(c,d)) = bounds mat
        imag = map (\(i,_) -> array (b,d) [(j,mat'!(i,j)) | j <- [b..d]]) $ Map.toList pto
        nonpivs = [a..c]\\(map fst $ Map.toList pto)
        kern = map (\k -> let ons = onNums k in array (a,c) [(i,ZMod2 $ Set.member i ons) | i <- [a..c]]) nonpivs
        onNums k = Set.fromList $ k : (mapMaybe (\j -> if 1==(mat!(k,j)) then Map.lookup j pfrom else Nothing) [b..d])

prettyShowMatrix :: (Show r, IArray UArray r) => Matrix r  -> String
prettyShowMatrix mat = cim "\n" (\j -> "[" ++ (cim " " (\i -> show $ mat!(i,j)) [a..c]) ++ "]") [b..d]
  where ((a,b),(c,d)) = bounds mat
prettyPrintMatrix mat = putStrLn $ prettyShowMatrix mat


fromLists :: (IArray UArray r) => [[r]] -> Matrix r
fromLists lsts = array ((1,1),(length $ head lsts, length lsts))
                 $ concat $ zipWith (\i l -> zipWith (\j x -> ((j,i),x)) [1..] l) [1..] lsts

tstMat1 :: Matrix ZMod2
tstMat1 = fromLists [
  [1,1],
  [1,0],
  [1,1]]

