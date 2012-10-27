module Utils where

import Data.List
import System.Time
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array

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


safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs


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

componantOf :: Integer -> Integer -> Integer
componantOf n k 
  | divisible n k = n * (componantOf n (k`div`n))
  | otherwise     = 1

divisible k n = 0 == n `mod` k 

average lst = (fromInteger $ sum lst) / (fromIntegral $ length lst)

-- and this
dumbError = error "dumb error"
nyi = error "Not yet implemented"

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

firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM f [] = return Nothing
firstJustM f (a:as) = do 
  a' <- f a
  if isNothing a' 
    then firstJustM f as 
    else return $ a'

-- and some list stuff
dropLast n = reverse . (drop n) . reverse

splitSubstr str ls = sst [] ls
  where sst fst [] = (reverse fst,[])
        sst fst rst = if take n rst == str
                      then (reverse fst,drop n rst)
                      else sst ((head rst):fst) (tail rst)
        n = length str

zipMap f ls = zip ls $ map f ls

mapHaltM pred f [] = return $ Left []
mapHaltM pred f (a:as) = do
  fa <- f a
  case pred $ fa of
    (Just a) -> return $ Right a
    Nothing -> do
      frst <- mapHaltM pred f as
      case frst of 
        (Left fas) -> return $ Left $ fa:fas
        res -> return $ res

maybeIf pred thn = if pred then Just thn else Nothing

sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

ixMapToArray mp = array (minimum $ Map.elems mp, maximum $ Map.elems mp) $ map (\(i,j) -> (j,i)) $ Map.toList mp 

removeOne pred lst = (head rst,fst ++ (tail rst))
  where (fst,rst) = break pred lst
       