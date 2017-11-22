{-# LANGUAGE MagicHash #-}

module InInstanceHead where

class C# a where          {-* MagicHash *-}
  f :: a -> a

instance (C#) Int where   {-* MagicHash *-}
  f = id
