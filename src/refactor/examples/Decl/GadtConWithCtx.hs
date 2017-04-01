{-# LANGUAGE GADTs #-}
module Decl.GadtConWithCtx where

data Concurrently m a where
  Concurrently :: Monad m => { runConcurrently :: m a } -> Concurrently m a
