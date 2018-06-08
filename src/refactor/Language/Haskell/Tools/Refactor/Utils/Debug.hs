


module Language.Haskell.Tools.Refactor.Utils.Debug
  ( module Language.Haskell.Tools.Refactor.Utils.Debug
  , module Debug.Trace
  ) where

import Data.Maybe (isJust)
import Control.Monad.Trans.Maybe
import Control.Reference ((^.), (&))

import Debug.Trace
import qualified Outputable as GHC

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite


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

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showSDocUnsafe . GHC.ppr

showName :: Name -> String
showName = (^. simpleName & unqualifiedName & simpleNameStr)

showOp :: Operator -> String
showOp = (^. operatorName & unqualifiedName & simpleNameStr)
