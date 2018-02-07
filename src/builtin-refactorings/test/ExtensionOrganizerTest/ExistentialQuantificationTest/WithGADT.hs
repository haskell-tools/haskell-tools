{-# LANGUAGE ExistentialQuantification, GADTSyntax #-}

module WithGADT where

{-@ ExistentialQuantification, GADTSyntax @-}

data T a = forall c . T a c  {-* ExplicitForAll, GADTs + ExistentialQuantification *-}

data GT a where
  GT1 :: a -> GT [a]         {-* GADTSyntax, GADTs + ExistentialQuantification *-}
