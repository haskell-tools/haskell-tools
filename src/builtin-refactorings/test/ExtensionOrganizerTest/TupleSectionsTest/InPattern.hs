{-# LANGUAGE TupleSections,
             ViewPatterns
             #-}

module InPattern where

f (fst . (,0) -> []) = ()   {-* TupleSections, ViewPatterns *-}
f (fst . (,0) -> ys) = ()   {-* TupleSections, ViewPatterns *-}
