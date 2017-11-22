{-# LANGUAGE ViewPatterns #-}

module InAlt where

f g x = case x of
          (g -> [])   -> () {-* ViewPatterns *-}
          (g -> x:xs) -> () {-* ViewPatterns *-}
