{-# LANGUAGE NoImplicitPrelude #-}
module Refactor.DollarApp.Final where

import Prelude ((+))

import A

x = f (g 1)
x2 = f (f (g 2))

x3 = f (g 3) + 3
x4 = 3 + f (g 4)

x5 = f (g $$ 1)
x6 = f (1 + 1)

