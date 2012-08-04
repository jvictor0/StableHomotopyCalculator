module Timing where

import System.CPUTime
import Control.Monad
import System.Environment
import Control.Exception
import Data.Time

time obj = do
  startTime <- getCPUTime
  case obj == obj of
    True -> return ()
    False -> putStr "" 
  endTime <- getCPUTime
  return $ endTime - startTime

psToTime = picosecondsToDiffTime

timeIO obj = do
  startTime <- getCPUTime
  a <- obj
  case a == a of
    True -> return ()
    False -> putStr "" 
  endTime <- getCPUTime
  return (endTime - startTime)
  