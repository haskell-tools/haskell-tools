{-# LANGUAGE BangPatterns,
             PatternSynonyms
             #-}

module Combined where


asd = [1..]
  where
    f !x  {-* BangPatterns *-}
      | ((!y):ys) <- x = case y of {-* BangPatterns *-}
                           (!z):zs -> 5 {-* BangPatterns *-}
                             where (_,!a) = (1,2) {-* BangPatterns *-}

    m = do
      (!a,_) <- Just (1,2) {-* BangPatterns *-}
      return a
