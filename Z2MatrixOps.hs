{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
module Z2MatrixOps where

import Data.Bits hiding (xor)
import Timing
import System.CPUTime
import System.Random
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
import Data.Word


-- this module deserves some cleaning up

-- for whatever unkown reason, these functions are extremely slow.
-- the results of running these functions has aleady been generated, so no worries there, but it is sad, and this needs to be fixed.  


addColumn ::  (STUArray s (Int,Int) Bool) -> ((Int,Int),(Int,Int)) -> ST s ()
addColumn arr ((a,c),(source,target)) = do
  forM_ [a..c] $ \i -> do
    src <-  readArray arr (i,source)
    targ <- readArray arr (i,target)
    writeArray arr (i,target) (src `xor` targ)


thawToZ2Mat :: Matrix ZMod2 -> ST s (STUArray s (Int,Int) Word64)
thawToZ2Mat mat = do
  result <- newArray ((0,b),(newLen,d)) 0
  forM [((i,j),foldr (\k res -> if (inRange  ((a,b),(c,d))(a+64*i+k,j)) && (mat!(a+64*i+k,j)==1) then setBit res k else res) 0 [0..63])
              | j <- [b..d], i <- [0..newLen]] $ \((i,j),wrd) -> writeArray result (i,j) wrd
  return result
  where ((a,b),(c,d)) = bounds mat
        newLen = (c-a+1)`div` 64

freezeZ2Mat :: (STUArray s (Int,Int) Word64) -> ((Int,Int),(Int,Int)) -> ST s (Matrix ZMod2)
freezeZ2Mat mat ((a,b),(c,d)) = do
  ls <- sequence [readZ2Mat mat (i,j) a >>= (\v ->return ((i,j),ZMod2 v)) | i <- [a..c], j <- [b..d]]
  return $ array ((a,b),(c,d)) ls

readZ2Mat :: (STUArray s (Int,Int) Word64) -> (Int,Int) -> Int -> ST s Bool
readZ2Mat mat (i,j) a = readArray mat ((i-a)`div`64,j) >>= (\v -> return $ testBit v $ (i-a)`mod`64)


testZ2FreezeThaw g i j = runST $ do
  let mat = randMat g i j
  let bds = bounds mat
  thawMat <- thawToZ2Mat mat
  freezeMat <- freezeZ2Mat thawMat bds
  return $ mat == freezeMat
    
rrefWPivs :: Matrix ZMod2 -> (Matrix ZMod2,Map.Map Int Int, Map.Map Int Int)
rrefWPivs arr' =  runST $ do 
  let ((a,b),(c,d)) = bounds arr'
  pivs <- newSTRef $ (Map.empty,Map.empty)
  arr <- thaw $ (amap (==1)) arr'
  forM_ [a..c] $ \i -> do
    (to,from) <- readSTRef pivs
    fnd <-  findM (\j -> fmap ((&&(not $ Map.member j from))) (readArray arr (i,j))) [b..d] 
    case fnd  of
      Nothing -> return ()
      (Just j) -> do
        modifySTRef pivs $ \(to,from) ->  (Map.insert i j to, Map.insert j i from)
        forM_ ([b..j-1]++[j+1..d]) $ \k -> do
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
        nonpivs = filter (not . (flip Map.member pto)) [a..c]
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

preimages :: Vector ZMod2 -> Matrix ZMod2 -> [Vector ZMod2]
preimages v vs = map (\u -> array (a,c) [(j,u!j) | j <- [a..c]]) preres
  where mat = array ((a-1,b),(c, d)) $ [((a-1,j), v!j) | j <- [b..d]] ++ (assocs vs)
        ((a,b),(c,d)) = bounds vs
        (_,kern,_,_) = imageKernel mat
        preres = filter (\u -> (u!(a-1)) /= 0) kern

preimage v vs = head $ preimages v vs

vin v vs = [] /= (preimages v $ array ((1,a),(length vs,b)) $ 
                   (concat $ zipWith (\i u -> [((i,j),u!j) | j <- [a..b]]) [1..] vs))
  where (a,b) = bounds v

randMat :: StdGen -> Int -> Int -> Matrix ZMod2
randMat g m n= array ((1,1),(m,n)) $ zip (range ((1,1),(m,n))) $ randoms g

randMatIO m n = randomIO >>= (\g -> return $ randMat (mkStdGen g) m n)

timeRref m n = 
  fmap (average) $ mapM (const $ timeIO $ fmap rref $ randMatIO m n) [1..100]

timeRrefs :: (Double -> Double) -> IO ()
timeRrefs poss = do
  mapM_ (\i -> timeRref i i >>= (\r -> putStrLn $ (show i) ++ ": " ++ (show r) ++ " -- " ++ (show $ r/(poss $ fromIntegral i)))) [1..]
