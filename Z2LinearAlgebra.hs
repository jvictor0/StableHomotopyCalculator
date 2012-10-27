module Z2LinearAlgebra where

import Z2MatrixOps
import LinearAlgebra
import Data.Array.Unboxed
import Data.List
import Module

decomposeBasis v [] = error $ "empty basis, decompising " ++ (show v)
decomposeBasis v basis = zipWith (*>) (elems $  preimage (decompose domain v) (vlistToMatrix $ map (decompose domain) basis)) basis
  where domainlst = nub $ concatMap ((map fst).toAList) $ v:basis
        domain = listArray (1,length domainlst) $ domainlst
        
