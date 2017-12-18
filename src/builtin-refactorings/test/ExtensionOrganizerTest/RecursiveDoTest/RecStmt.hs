{-# LANGUAGE RecursiveDo #-}

module RecStmt where

justOnes = do { rec { xs <- Just (1:xs) } {-* RecursiveDo *-}
              ; return (map negate xs) }
