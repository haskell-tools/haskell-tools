{-# LANGUAGE MagicHash #-}

module InFieldDecl where

import GHC.Prim

data Rec a# = R               {-* MagicHash *-}
              { f1# :: Int#   {-* MagicHash, MagicHash *-}
              , f2# :: a#     {-* MagicHash, MagicHash *-}
              }
