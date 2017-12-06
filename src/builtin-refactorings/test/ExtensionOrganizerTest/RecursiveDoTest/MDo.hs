{-# LANGUAGE RecursiveDo #-}

module MDo where

justOnes = mdo { xs <- Just (1:xs)
               ; return (map negate xs) } {-* RecursiveDo *-}
