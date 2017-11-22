{-# LANGUAGE BangPatterns #-}

module InAlt where

f x = case x of
        (!a,!b) -> a + b    {-* BangPatterns, BangPatterns *-}
