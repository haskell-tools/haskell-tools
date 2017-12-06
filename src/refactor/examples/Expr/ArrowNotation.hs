{-# LANGUAGE Arrows #-}
module Expr.ArrowNotation where

import Control.Arrow

addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = proc x -> do
                y <- f -< x
                z <- g -< x
                returnA -< y + z