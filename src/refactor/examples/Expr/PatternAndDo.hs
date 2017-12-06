{-# LANGUAGE RecordWildCards #-}
module Expr.PatternAndDo where

import Control.Monad
import Control.Monad.Identity

data A = A { x :: Int }

a = forM_ [] $ \A {..} -> do
      Identity "Hello"
