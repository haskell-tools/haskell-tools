{-# LANGUAGE LambdaCase #-}

module InRhs where

f [] = \case { [] -> ()}    {-* LambdaCase *-}
f x  = \case { [] -> ()}    {-* LambdaCase *-}
