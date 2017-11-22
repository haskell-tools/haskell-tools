{-# LANGUAGE LambdaCase #-}

module InCaseRhs where

f x = case x of
        [] -> \case {() -> ()}    {-* LambdaCase *-}
        xs -> \case {() -> ()}    {-* LambdaCase *-}
