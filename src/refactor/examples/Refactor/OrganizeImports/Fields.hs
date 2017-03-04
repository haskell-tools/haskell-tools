module Refactor.OrganizeImports.Fields where

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (Monad(..), StateT(runStateT))

x = runStateT (lift putStrLn >> return ()) ()
