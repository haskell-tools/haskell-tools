module Expr.DoNotation where

import Control.Monad.Identity

x1 :: Identity ()
x1 = return ()

x2 :: Identity ()
x2 = do return ()

x3 :: Identity ()
x3 = do { return () }

x4 :: Identity Int
x4 = do { one <- Identity 1; return (one + 1) }
