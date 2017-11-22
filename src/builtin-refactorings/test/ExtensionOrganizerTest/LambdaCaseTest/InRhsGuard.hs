{-# LANGUAGE LambdaCase #-}

module InRhsGuard where

f x
  | y1 <- (\case {() -> ()}) x,   {-* LambdaCase *-}
    y2 <- (\case {() -> ()}) y1   {-* LambdaCase *-}
  = ()
  | z1 <- (\case {() -> ()}) x    {-* LambdaCase *-}
  = ()
