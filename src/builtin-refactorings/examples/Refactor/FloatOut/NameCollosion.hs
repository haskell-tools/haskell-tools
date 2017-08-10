module Refactor.FloatOut.NameCollosion where

f = g
  where g = id

g = ()