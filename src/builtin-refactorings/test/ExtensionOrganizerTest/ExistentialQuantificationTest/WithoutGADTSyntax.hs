{-# LANGUAGE ExistentialQuantification #-}

module WithoutGADTSyntax where

{-@ ExistentialQuantification @-}

data T1 a = forall c . T1 a c  {-* ExplicitForAll, GADTs + ExistentialQuantification *-}
data T2 a = Eq a => T2 a       {-* GADTs + ExistentialQuantification *-}
