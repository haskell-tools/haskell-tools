{-# LANGUAGE BangPatterns #-}

module InMatchLhs where

f !x = x  {-* BangPatterns *-}
