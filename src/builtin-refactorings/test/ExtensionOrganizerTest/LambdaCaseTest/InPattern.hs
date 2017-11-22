{-# LANGUAGE LambdaCase,
             ViewPatterns
             #-}

module InPattern where

f (\case {[] -> []} -> []) = ()   {-* LambdaCase, ViewPatterns *-}
f (\case {xs -> xs} -> ys) = ()   {-* LambdaCase, ViewPatterns *-}
