{-# LANGUAGE MagicHash #-}

module InMatchLhs where

f :: [a] -> [a]
f (a# : as#) = as#  {-* MagicHash, MagicHash, MagicHash *-}
f [a#] = [a#]       {-* MagicHash, MagicHash *-}
