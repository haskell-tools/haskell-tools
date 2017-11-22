{-# LANGUAGE BangPatterns #-}

module InRhsGuard where

f x
  | [!y] <- x = 5   {-* BangPatterns *-}
