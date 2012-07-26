module ZMod2 where

import Utils

data ZMod2 = ZMod2 Bool deriving (Eq,Ord)

instance Show ZMod2 where
  show (ZMod2 b) = if b then "1" else "0"

biFunct f (ZMod2 x) (ZMod2 y) = ZMod2 $ f x y

instance Num ZMod2 where
  (+) = biFunct xor
  negate = id
  (*) = biFunct (&&)
  fromInteger n = ZMod2 $ even n
  abs = dumbError
  signum = dumbError