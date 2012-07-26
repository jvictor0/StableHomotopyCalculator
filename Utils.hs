module Utils where

import Data.List

xor True = not
xor False = id


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


-- and this
dumbError = error "dumb error"