module Type.Unpack where

data X = X {-# UNPACK #-} !Int {-# NOUNPACK #-} !Int 

