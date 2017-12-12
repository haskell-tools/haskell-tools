module Control.Instances.Test where

import Control.Instances.Morph

import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.State

test1 :: Identity a -> IO a
test1 = morph

test2 :: Maybe a -> [a]
test2 = morph

test3 :: Maybe a -> ListT IO a
test3 = morph

test4 :: Maybe a -> MaybeT IO a
test4 = morph

test5 :: Monad m => Maybe a -> ListT (StateT s m) a
test5 = morph