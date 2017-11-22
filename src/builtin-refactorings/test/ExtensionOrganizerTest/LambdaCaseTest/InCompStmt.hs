{-# LANGUAGE LambdaCase #-}

module InCompStmt where

xs = [ (\case {_ -> ()}) x | x <- [1..10] ] {-* LambdaCase *-}

ys = [ 2*y | y <- (\case {x -> x}) [1..10] ]  {-* LambdaCase *-}

zs = [ (\case {_ -> ()}) z | z <- (\case {x -> x}) [1..10] ]  {-* LambdaCase, LambdaCase *-}
