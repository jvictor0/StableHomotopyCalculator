{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module ZMod2 where

import Utils
import Data.Array.Unboxed
import System.Random

newtype ZMod2 = ZMod2 Bool deriving (IArray UArray, Eq, Ord)

instance Show ZMod2 where
  show (ZMod2 b) = if b then "1" else "0"

instance Num ZMod2 where
  (+) = ZMod2 .-. (xor `on` (==1))
  negate = id
  (*) = ZMod2 .-. ((&&) `on` (==1))
  fromInteger = ZMod2 . odd
  abs = dumbError
  signum = dumbError

instance Random ZMod2 where
  random g = let (r,g') = random g in (ZMod2 r,g')
  randomR (ZMod2 x,ZMod2 y) g = let (r,g') = randomR (x,y) g in (ZMod2 r, g')

{-
{-# INLINE from #-}
from :: UArray a ZMod2 -> UArray a ZMod2
from = unsafeCoerce
{-# INLINE to #-}
to   :: UArray a ZMod2 -> UArray a ZMod2
to   = unsafeCoerce
{-# INLINE func #-}
func :: (ZMod2 -> a -> ZMod2) -> (ZMod2 -> a -> ZMod2)
func = unsafeCoerce

instance IArray UArray ZMod2 where
  {-# INLINE bounds #-}
  bounds = bounds . from
  {-# INLINE numElements #-}
    numElements = numElements .from
    {-# INLINE unsafeArray #-}
    unsafeArray lu = to . unsafeArray lu .  unsafeCoerce
    {-# INLINE unsafeAt #-}
    unsafeAt = unsafeCoerce . unsafeAt . from
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr =  to . unsafeReplace (from arr) . unsafeCoerce
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr =  to . unsafeAccum (func f) (from arr)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu =  to . unsafeAccumArray (func f) (unsafeCoerce initialValue) lu -}