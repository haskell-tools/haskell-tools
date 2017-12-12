{-# LANGUAGE GADTs, KindSignatures, DeriveDataTypeable #-}
module Decl.GADT where

import Data.Typeable

data DMap k f where
    Tip :: DMap k f
    Bin :: !Int -> !(k v) -> f v -> !(DMap k f) -> !(DMap k f) -> DMap k f
    deriving Typeable
