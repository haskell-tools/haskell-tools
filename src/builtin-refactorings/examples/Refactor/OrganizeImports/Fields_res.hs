module Refactor.OrganizeImports.Fields where

import Control.Monad.State (Monad(..), StateT(runStateT))
import Control.Monad.Trans (MonadTrans(..))

x = runStateT (lift putStrLn >> return ()) ()
