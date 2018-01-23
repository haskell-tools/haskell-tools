module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.Debug
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.Debug
  , module Debug.Trace
  ) where

import Control.Monad.Trans.Maybe
import Data.Maybe (isJust)

import Debug.Trace

debugM :: (Monad m, Show a) => m a -> m a
debugM m = do
  x <- m
  traceShow x m

debug :: Show a => a -> a
debug x = traceShow x x

-- | Displays True iff the wrapped value is a Just
debugMaybeT :: Monad m => MaybeT m a -> MaybeT m a
debugMaybeT m = MaybeT $ do
  x <- runMaybeT m
  traceShow (isJust x) (return x)
