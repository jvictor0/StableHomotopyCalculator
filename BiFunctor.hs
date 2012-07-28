module BiFunctor where


class BiFunctor f where
  bmap :: (a -> b) -> (c -> d) -> f a c -> f b d
  