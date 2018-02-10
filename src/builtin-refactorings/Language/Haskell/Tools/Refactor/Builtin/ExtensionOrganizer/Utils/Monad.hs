module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.Monad where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..))

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

maybeT :: Monad m => b -> (a -> b) -> MaybeT m a -> m b
maybeT def f x = liftM (maybe def f) (runMaybeT x)

maybeTM :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeTM def f x = runMaybeT x >>= maybe def f
