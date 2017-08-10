-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Types where

import HsTypes as GHC (HsType)
import Language.Haskell.Tools.AST as AST (UType, Dom, RangeStage)
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName)

trfType' :: TransformName n r => HsType n -> Trf (AST.UType (Dom r) RangeStage)
