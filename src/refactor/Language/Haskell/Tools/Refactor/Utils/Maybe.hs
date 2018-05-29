module Language.Haskell.Tools.Refactor.Utils.Maybe
 ( module Language.Haskell.Tools.Refactor.Utils.Maybe
 , module Data.Maybe
 , MaybeT(..)
 ) where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..))

isJustT :: Monad m => MaybeT m a -> m Bool
isJustT m = liftM isJust . runMaybeT $ m

isNothingT :: Monad m => MaybeT m a -> m Bool
isNothingT m = liftM isNothing . runMaybeT $ m

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

fromMaybeT :: Monad m => a -> MaybeT m a -> m a
fromMaybeT def = maybeT def id

fromMaybeTM :: Monad m => m a -> MaybeT m a -> m a
fromMaybeTM def = maybeTM def return

maybeT :: Monad m => b -> (a -> b) -> MaybeT m a -> m b
maybeT def f x = liftM (maybe def f) (runMaybeT x)

maybeTM :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeTM def f x = runMaybeT x >>= maybe def f
