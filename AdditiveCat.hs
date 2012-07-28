module AdditiveCat where

instance (Num x) => Num (a -> x) where
  (f + g) x = (f x) + (g x)
  negate = (negate .)
  (f * g) x = (f x) * (g x)
  fromInteger = const . fromInteger
  signum = (signum .)
  abs = (abs .)


  