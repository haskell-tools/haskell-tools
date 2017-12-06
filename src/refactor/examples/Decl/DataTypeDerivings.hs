module Decl.DataTypeDerivings where

data A = A deriving (Eq, Show)
data B = B deriving (Eq)
data C = C deriving Eq