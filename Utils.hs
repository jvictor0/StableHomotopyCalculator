module Utils where

import Data.List

xor True = not
xor False = id


(f `on` g) x y = f (g x) (g y)

(f .-. g) x y = f $ g x y

-----------------------------------
-- some functions for printing ----
-----------------------------------

-- cim = concat-intersperse-map
cim t f l = concat $ intersperse t $ map f l


rshow s = if elem '+' ss then "(" ++ ss ++ ")" else ss
  where ss = show s


-- for printing latex numbers with {}
texShow s = if length ss > 1 then "{" ++ ss ++ "}" else ss
  where ss = show s



-- some easy math
fact :: Int -> Integer
fact n = product [1..fromIntegral n]

choose 0 0 = 1
choose 0 k = 0
choose n k
  | k > n || n < 0 || k < 0  = 0
choose n k = (fact n) `div` ((fact k) * (fact $ n - k))

isPowerOf :: Int -> Int -> Bool
isPowerOf 1 _ = True
isPowerOf n k
  | divisible k n  = isPowerOf (n `div` k) k 
  | otherwise      = False

divisible k n = 0 == n `mod` k 

-- and this
dumbError = error "dumb error"


-- simple class
class UnShow u where
  unShow :: String -> u



-- Monadic Utils
findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f [] = return Nothing
findM f (x:xs) = do
  b <- f x
  if b then return (Just x) else findM f xs

ifM :: (Monad m) => (m Bool) -> (m a) -> (m a) -> (m a)
ifM b t e = b >>= (\b' -> if b' then t else e)


-- and some list stuff
dropLast n = reverse . (drop n) . reverse

splitSubstr str ls = sst [] ls
  where sst fst [] = (reverse fst,[])
        sst fst rst = if take n rst == str
                      then (reverse fst,drop n rst)
                      else sst ((head rst):fst) (tail rst)
        n = length str