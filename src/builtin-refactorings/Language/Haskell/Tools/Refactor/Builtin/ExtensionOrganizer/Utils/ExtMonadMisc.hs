{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.ExtMonadMisc where

import SrcLoc (SrcSpan, mkGeneralSrcSpan)

import qualified Data.Map.Strict as SMap (Map(..), empty, insertWith)

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad


addOccurenceNoLoc' :: Ord k => k ->  SMap.Map k [SrcSpan] -> SMap.Map k [SrcSpan]
addOccurenceNoLoc' key = SMap.insertWith (++) key [mkGeneralSrcSpan ""]

addOccurenceNoLoc :: MonadState ExtMap m => Extension -> m ()
addOccurenceNoLoc extension = modify $ addOccurenceNoLoc' (LVar extension)
