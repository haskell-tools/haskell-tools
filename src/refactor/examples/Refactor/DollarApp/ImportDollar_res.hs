{-# LANGUAGE NoImplicitPrelude #-}
module Refactor.DollarApp.ImportDollar where

import Refactor.DollarApp.Defs
import qualified GHC.Base(($))

x = f GHC.Base.$ g x
