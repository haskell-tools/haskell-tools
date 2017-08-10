module Refactor.FloatOut.SharedSignature where

f = g
  where g, h :: a -> a
        g = id
        h = id