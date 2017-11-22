{-# LANGUAGE MagicHash,
             FunctionalDependencies
             #-}

module InFunDeps where

class C a# b# | a# -> b# where  {-* MagicHash, MagicHash, MagicHash, MagicHash *-}
  f :: a# -> b#                 {-* MagicHash, MagicHash *-}
