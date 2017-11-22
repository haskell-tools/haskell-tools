{-# LANGUAGE MagicHash #-}

module InFieldUpdate where

-- TODO: non-exhaustive

data Rec = Rec { x# :: Int }  {-* MagicHash *-}

g :: Rec -> Rec
g Rec{x# = x} = Rec {x# = 2}  {-* MagicHash, MagicHash *-}
