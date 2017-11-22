{-# LANGUAGE MagicHash #-}

module InTypeSig where

-- non-exhaustive

f# :: a -> a            {-* MagicHash *-}
f# = id                 {-* MagicHash *-}
  where asd# :: b -> b  {-* MagicHash *-}
        asd# = id       {-* MagicHash *-}

class C a where
  g# :: a -> a   {-* MagicHash *-}
