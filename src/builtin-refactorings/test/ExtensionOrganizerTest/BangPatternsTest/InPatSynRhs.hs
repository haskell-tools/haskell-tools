{-# LANGUAGE PatternSynonyms,
             BangPatterns
             #-}

module InPatSynRhs where

pattern Bi a b = (!a,b)    {-* BangPatterns, PatternSynonyms *-}

pattern Uni x <- (!x):xs   {-* BangPatterns *-}
  where Uni !x = [x]       {-* BangPatterns, PatternSynonyms *-}
