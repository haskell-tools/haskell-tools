{-# LANGUAGE MagicHash,
             DefaultSignatures
             #-}

module InClassElement where

class C a where
  f# :: a -> a          {-* MagicHash *-}
  default f# :: a -> a  {-* MagicHash *-} -- DefaultSignatures
  f# = id               {-* MagicHash *-}
