{-# LANGUAGE MagicHash,
             GADTs
             #-}

module InDeclHead where

type T1# = ()           {-* MagicHash *-}
newtype T2# = T2# T1#   {-* MagicHash, MagicHash, MagicHash *-}
data T3# = T31# | T32#  {-* MagicHash, MagicHash, MagicHash *-}

data Expr# a where                              {-* MagicHash *-} -- GADTS
    I   :: Int  -> Expr# Int                    {-* MagicHash *-}
    B   :: Bool -> Expr# Bool                   {-* MagicHash *-}
    Add :: Expr# Int -> Expr# Int -> Expr# Int  {-* MagicHash, MagicHash, MagicHash *-}
    Mul :: Expr# Int -> Expr# Int -> Expr# Int  {-* MagicHash, MagicHash, MagicHash *-}
    Eq  :: Expr# Int -> Expr# Int -> Expr# Bool {-* MagicHash, MagicHash, MagicHash *-}

class C1# a where  {-* MagicHash *-}
  f :: a -> a
