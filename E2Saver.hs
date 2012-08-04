module E2Saver where

import E2Calculator
import System.Directory
import Utils
import qualified Data.Map as Map
import SteenrodAlgebra
import E2Gen
import Data.Array
import System.IO
import Module
import Control.Monad
import System.Time
import LinearAlgebra
import Tensor
import Z2MatrixOps
import Timing

outputFileName = "E2DataFile.e2"

clearData :: IO ()
clearData = do
  exists <- doesFileExist outputFileName
  if exists then removeFile outputFileName else return ()

loadE2Page :: Int -> IO E2GenData
loadE2Page ld = do
  putStrLn $ "Loading E2 Page for t - s < " ++ (show $ ld + 1)
  exists <- doesFileExist outputFileName
  if exists
    then do
    f <- openFile outputFileName ReadMode
    [ld_old] <- fmap ((map read).words) $ hGetLine f :: IO [Int]
    putStrLn $ "We have data up to " ++ (show ld_old)
    conts <- fmap ((map words).lines)  $ hGetContents f
    let dt_old = readE2Data conts ld_old
    let result =  extendE2Data dt_old ld
    putStrLn $ "There are " ++ (show $ length conts) ++ " known generators"
    hClose f
    when (ld_old < ld) $ reportE2Page  result
    return result
    else do
    putStrLn "no data found"
    let result = calculateE2Page ld
    reportE2Page result
    return result

readE2Data :: [[String]] -> Int -> E2GenData
readE2Data lsts ld = E2GD {largestDegree = ld,
                           gensDiffMap = array ((0,0),(ld,ld))
                                         [((s,t_s),Map.filterWithKey (\(E2Gen s' t_s' _) _ -> (s,t_s) == (s',t_s')) tup)
                            | s <- [0..ld], t_s <- [0..ld]],
                           knownGens = map fst $ Map.toList tup}
  where tup = Map.fromList $ map (\(hed:terms) -> (unShow hed, sum $ map (toFModule . unShow) terms)) lsts

reportE2Page :: E2GenData -> IO ()
reportE2Page dta = do
  totalStartTime <- getClockTime
  let scb = admisArray $ (largestDegree dta)
  forM_ [0..(largestDegree dta)-2] $ \t_s -> do
    forM_ [1..(largestDegree dta)] $ \s -> do
      startTime <- getClockTime
      let inputSize = sum $ map (\j -> (Map.size$ (gensDiffMap dta)!(s,j)) * (length $ scb!(t_s-j))) [0..t_s-1]
      let outputSize = sum $ map (\j -> (Map.size$ (gensDiffMap dta)!(s-1,j)) * (length $ scb!(t_s-j+1))) [0..t_s]
      putStrLn $ "Calculating box " ++ (show (s,t_s))
      putStrLn $ "   Matrix at this block is " ++ (show inputSize) ++ " x " ++ (show outputSize)
      mapM_ (\(gen,targ) -> putStrLn $ "   - " ++ (show gen) ++ " |--> " ++ (show targ)) $ Map.toList $ (gensDiffMap dta)!(s,t_s)
      endTime <- getClockTime
      let dt =  (timeDiffToString $ diffClockTimes endTime startTime)
      when (dt /= "") $ putStrLn $ "   This calculation took " ++ dt
    putStrLn $ "Calculation complete through t - s = " ++ (show t_s)
  putStrLn "Saving E2 Page Data"
  clearData
  let outputStr = (show $ largestDegree dta) ++ "\n"
                  ++ (cim "\n" (\(gen,targ) -> (show gen) ++ " " ++ (cim " " (show . fst) $ toAList targ))
                      $ concat $  map Map.toList $ map snd $ assocs $ gensDiffMap dta)
  writeFile outputFileName outputStr
  totalEndTime <- getClockTime
  putStrLn $ "The computation finished in " ++ (timeDiffToString $ diffClockTimes totalEndTime totalStartTime)
  putStrLn "Save successful"



