{-# LANGUAGE ViewPatterns #-}

module InMatchLhsNested where

f x = ()
  where g h (h -> [])   = ()  {-* ViewPatterns *-}
        g h (h -> x:xs) = ()  {-* ViewPatterns *-}

        h (j, j -> [])  = ()  {-* ViewPatterns *-}
