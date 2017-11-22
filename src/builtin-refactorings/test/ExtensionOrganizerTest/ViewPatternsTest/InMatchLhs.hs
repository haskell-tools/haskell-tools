{-# LANGUAGE ViewPatterns #-}

module InMatchLhs where

f (id -> [])   = () {-* ViewPatterns *-}
f (id -> x:xs) = () {-* ViewPatterns *-}
