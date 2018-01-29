{-# LANGUAGE FlexibleContexts, RankNTypes, StandaloneDeriving, TypeFamilies, TypeSynonymInstances #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.TH.LanguageExtensions
  , module Control.Monad.State
  , module Control.Monad.Reader
  ) where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap

import GHC (SrcSpan(..), Ghc(..), runGhc)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as SMap (Map(..), empty, insertWith)


{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Use import/export shortcut" #-}

deriving instance Ord  Extension
deriving instance Read Extension


-- how could I hide the tyvar a?
-- type Asd a = forall m . (MonadReader [Extension] m, MonadState ExtMap m, GhcMonad m) => m a


type ExtMonad = ReaderT [Extension] (StateT ExtMap Ghc)

type CheckNode  elem  = elem -> ExtMonad elem
type CheckUNode uelem = Ann uelem IdDom SrcTemplateStage -> ExtMonad (Ann uelem IdDom SrcTemplateStage)

class Checkable node where
  check :: CheckNode node

addOccurence' :: (Ord k, HasRange a) =>
                 k -> a -> SMap.Map k [SrcSpan] -> SMap.Map k [SrcSpan]
addOccurence' key node = SMap.insertWith (++) key [getRange node]

-- TODO: add isTurnedOn check
addOccurence_ :: (MonadState ExtMap m, HasRange node) =>
                  Extension -> node -> m ()
addOccurence_ extension element = modify $ addOccurence' (LVar extension) element

addOccurence :: (MonadState ExtMap m, HasRange node) =>
                 Extension -> node -> m node
addOccurence ext node = addOccurence_ ext node >> return node

isTurnedOn :: Extension -> ExtMonad Bool
isTurnedOn ext = do
  defaults <- ask
  return $! ext `elem` defaults

conditional :: (node -> ExtMonad node) ->
               Extension ->
               node ->
               ExtMonad node
conditional checker ext = conditionalAny checker [ext]

conditionalNot :: (node -> ExtMonad node) ->
                  Extension ->
                  node ->
                  ExtMonad node
conditionalNot checker ext node = do
  b <-isTurnedOn ext
  if b then return node else checker node

conditionalAny :: (node -> ExtMonad node) ->
                   [Extension] ->
                   node ->
                   ExtMonad node
conditionalAny checker exts node = do
  bs <- mapM isTurnedOn exts
  if or bs then checker node else return node

conditionalAdd :: HasRange node => Extension -> node -> ExtMonad node
conditionalAdd ext = conditional (addOccurence ext) ext


runExtMonadIO :: ExtMonad a -> IO a
runExtMonadIO = runGhc (Just libdir) . runExtMonadGHC

runExtMonadGHC :: ExtMonad a -> Ghc a
runExtMonadGHC = liftM fst . flip runStateT SMap.empty . flip runReaderT []
