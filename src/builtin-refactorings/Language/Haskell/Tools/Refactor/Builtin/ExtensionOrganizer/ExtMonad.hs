{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.Tools.Refactor.Utils.Maybe
  , module Language.Haskell.TH.LanguageExtensions
  , module Control.Monad.State
  , module Control.Monad.Reader
  ) where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Utils.Maybe
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap

import GHC (SrcSpan(..), Ghc(..), runGhc)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as SMap (Map(..), empty, insertWith)


{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Use import/export shortcut" #-}


type ExtMonad = ReaderT [Extension] (StateT ExtMap Ghc)

type CheckNode  elem  = elem -> ExtMonad elem
type CheckUNode uelem = Ann uelem IdDom SrcTemplateStage -> ExtMonad (Ann uelem IdDom SrcTemplateStage)

class Checkable node where
  check :: CheckNode node

addHint' :: (Ord k, HasRange a) =>
                 k -> a -> SMap.Map k [Occurence SrcSpan] -> SMap.Map k [Occurence SrcSpan]
addHint' key node = SMap.insertWith (++) key [Hint (getRange node)]

addHint_ :: (MonadState ExtMap m, HasRange node) =>
             Extension -> node -> m ()
addHint_ extension element = modify $ addHint' (lVar extension) element

addHint :: (MonadState ExtMap m, HasRange node) =>
            Extension -> node -> m node
addHint ext node = addHint_ ext node >> return node

addRelationHint_ :: (MonadState ExtMap m, HasRange node) =>
                     LogicalRelation Extension -> node -> m ()
addRelationHint_ rel element = modify $ addHint' rel element

addRelationHint :: (MonadState ExtMap m, HasRange node) =>
                    LogicalRelation Extension -> node -> m node
addRelationHint rel node = addRelationHint_ rel node >> return node


addEvidence' :: (Ord k, HasRange a) =>
                 k -> a -> SMap.Map k [Occurence SrcSpan] -> SMap.Map k [Occurence SrcSpan]
addEvidence' key node = SMap.insertWith (++) key [Evidence (getRange node)]

addEvidence_ :: (MonadState ExtMap m, HasRange node) =>
                 Extension -> node -> m ()
addEvidence_ extension element = modify $ addEvidence' (lVar extension) element

addEvidence :: (MonadState ExtMap m, HasRange node) =>
                Extension -> node -> m node
addEvidence ext node = addEvidence_ ext node >> return node

addRelation_ :: (MonadState ExtMap m, HasRange node) =>
                 LogicalRelation Extension -> node -> m ()
addRelation_ rel element = modify $ addEvidence' rel element

addRelation :: (MonadState ExtMap m, HasRange node) =>
                LogicalRelation Extension -> node -> m node
addRelation rel node = addRelation_ rel node >> return node


addEvidenceLoc' :: Ord k => k -> SrcSpan -> SMap.Map k [Occurence SrcSpan] -> SMap.Map k [Occurence SrcSpan]
addEvidenceLoc' k loc = SMap.insertWith (++) k [Evidence loc]

addEvidenceLoc :: MonadState ExtMap m => Extension -> SrcSpan -> m ()
addEvidenceLoc ext loc = modify $ addEvidenceLoc' (lVar ext) loc

addRelationLoc :: MonadState ExtMap m => LogicalRelation Extension -> SrcSpan -> m ()
addRelationLoc rel loc = modify $ addEvidenceLoc' rel loc

isTurnedOn :: Extension -> ExtMonad Bool
isTurnedOn ext = do
  defaults <- ask
  return $! ext `elem` defaults

isTurnedOff :: Extension -> ExtMonad Bool
isTurnedOff ext = not <$> isTurnedOn ext

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
conditionalAdd ext = conditional (addEvidence ext) ext

runExtMonadIO :: ExtMonad a -> IO a
runExtMonadIO = runGhc (Just libdir) . runExtMonadGHC

runExtMonadGHC :: ExtMonad a -> Ghc a
runExtMonadGHC = liftM fst . flip runStateT SMap.empty . flip runReaderT []
