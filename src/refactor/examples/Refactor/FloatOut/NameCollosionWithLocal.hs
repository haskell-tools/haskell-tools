module Refactor.FloatOut.NameCollosionWithLocal where

f = g
  where g = h
          where h = id
        h = ()