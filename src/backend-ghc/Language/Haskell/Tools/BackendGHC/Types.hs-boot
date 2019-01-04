{-# LANGUAGE TypeFamilies #-}
-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Types where

import HsTypes as GHC (HsType)
import Language.Haskell.Tools.AST as AST (UType, Dom, RangeStage)
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName)
import HsExtension (GhcPass)
import GHC.Stack (HasCallStack)

trfType' :: (TransformName n r, n ~ GhcPass p, HasCallStack)  => HsType n -> Trf (AST.UType (Dom r) RangeStage)
