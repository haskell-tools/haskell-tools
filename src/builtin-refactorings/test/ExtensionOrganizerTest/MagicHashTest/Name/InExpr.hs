{-# LANGUAGE MagicHash #-}

module InExpr where

-- TODO: non-exhaustive

data Rec = Rec# { x :: Int }  {-* MagicHash *-}

f :: Int -> Int
f x# = x# + x#                 {-* MagicHash, MagicHash, MagicHash *-}

g :: Rec -> Rec
g Rec#{x = x} = Rec# {x = 2}  {-* MagicHash, MagicHash *-}
