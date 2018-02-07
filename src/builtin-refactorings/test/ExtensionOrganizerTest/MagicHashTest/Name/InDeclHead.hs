{-# LANGUAGE MagicHash,
             GADTSyntax
             #-}

module InDeclHead where

type T1# = ()           {-* MagicHash *-}
newtype T2# = T2# T1#   {-* MagicHash, MagicHash, MagicHash *-}
data T3# = T31# | T32#  {-* MagicHash, MagicHash, MagicHash *-}

data Expr# a where                           {-* MagicHash *-}
    I   :: Int  -> Expr# a                   {-* GADTSyntax, MagicHash *-}
    B   :: Bool -> Expr# a                   {-* GADTSyntax, MagicHash *-}
    Add :: Expr# Int -> Expr# Int -> Expr# a {-* GADTSyntax, MagicHash, MagicHash, MagicHash *-}
    Mul :: Expr# Int -> Expr# Int -> Expr# a {-* GADTSyntax, MagicHash, MagicHash, MagicHash *-}
    Eq  :: Expr# Int -> Expr# Int -> Expr# a {-* GADTSyntax, MagicHash, MagicHash, MagicHash *-}

class C1# a where  {-* MagicHash *-}
  f :: a -> a
