{-# LANGUAGE PatternSynonyms #-}

module BiDirectional where

pattern X :: a -> [a] {-* PatternSynonyms *-}
pattern X a = [a]     {-* PatternSynonyms *-}
