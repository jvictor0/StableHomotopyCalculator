module Timing where

import System.CPUTime
import Control.Monad
import System.Environment
import Control.Exception

time obj = do
  startTime <- getCPUTime
  case obj == obj of
    True -> return ()
    False -> putStr "" 
  endTime <- getCPUTime
  return (endTime - startTime)

timeIO obj = do
  startTime <- getCPUTime
  a <- obj
  case a == a of
    True -> return ()
    False -> putStr "" 
  endTime <- getCPUTime
  return (endTime - startTime)
  