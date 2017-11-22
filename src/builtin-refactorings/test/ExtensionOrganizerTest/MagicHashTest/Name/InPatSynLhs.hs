{-# LANGUAGE MagicHash,
             PatternSynonyms
             #-}

module InPatSynLhs where

pattern Uni# :: a -> b -> c -> (a,b,c)  {-* PatternSynonyms, MagicHash *-}
pattern Uni# a b c <- (a,b,c)           {-* PatternSynonyms, MagicHash *-}

pattern Bi# :: a -> [a]      {-* PatternSynonyms, MagicHash *-}
pattern Bi# a = [a]          {-* PatternSynonyms, MagicHash *-}
