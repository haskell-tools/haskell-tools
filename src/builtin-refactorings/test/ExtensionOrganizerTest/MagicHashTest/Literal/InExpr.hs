{-# LANGUAGE MagicHash #-}

module InExpr where

-- non-exhaustive check for Expr node location in AST

f :: ()
f = ()
  where
  x1 = 1#       {-* MagicHash *-}
  x2 = 1##      {-* MagicHash *-}
  x3 = 3.14#    {-* MagicHash *-}
  x4 = 3.14##   {-* MagicHash *-}
  x5 = 'c'#     {-* MagicHash *-}
  x6 = "xxx"#   {-* MagicHash *-}
