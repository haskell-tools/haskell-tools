module Language.Haskell.Tools.AST.FromGHC.Binds where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Binds as AST

trfLocalBinds :: TransformName n => HsLocalBinds n -> Trf (AnnList AST.LocalBind (AnnotType n))
trfWhereLocalBinds :: TransformName n => HsLocalBinds n -> Trf (AnnMaybe AST.LocalBinds (AnnotType n))
trfRhsGuard :: TransformName n => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.RhsGuard (AnnotType n))
