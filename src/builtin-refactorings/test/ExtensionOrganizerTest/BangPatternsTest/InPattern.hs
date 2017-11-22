{-# LANGUAGE BangPatterns,
             ViewPatterns
             #-}

module InPattern where


data Rec = Rec { num :: Int}

(!x):(!xs) = [1..]  {-* BangPatterns, BangPatterns *-}

Just (Just !y) = Just $ Just 5  {-* BangPatterns *-}

(!a1,!a2,!a3) = (1, 2, 3) {-* BangPatterns, BangPatterns, BangPatterns *-}

[!b1, !b2] = [1,2]  {-* BangPatterns, BangPatterns *-}

f :: Rec -> Int
f Rec { num = !num } = num  {-* BangPatterns *-}

g x@(!y) = () {-* BangPatterns *-}

h (id -> !x) = () {-* BangPatterns, ViewPatterns *-}
