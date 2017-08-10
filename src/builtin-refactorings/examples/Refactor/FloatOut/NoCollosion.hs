module Refactor.FloatOut.NoCollosion where

f = g
  where g = h
          where h = id

h = ()