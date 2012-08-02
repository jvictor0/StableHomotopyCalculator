module Main where

import Control.Monad
import E2Saver

main = mapM_ loadE2Page [5,10..60]