module Expr.SemicolonDo where

import Control.Monad.Identity

a = do { n <- Identity ()
            ; return ()
            }
