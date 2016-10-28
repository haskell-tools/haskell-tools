-- | A custom AST representation for Haskell tools.
-- Different layers of the AST are recursive, to separate them into modules
-- we introduced source imports.
module Language.Haskell.Tools.AST 
  ( module Exported
  ) where

import Language.Haskell.Tools.AST.Instances

import Language.Haskell.Tools.AST.References as Exported
import Language.Haskell.Tools.AST.Helpers as Exported

import Language.Haskell.Tools.AST.Representation.Modules as Exported
import Language.Haskell.Tools.AST.Representation.TH as Exported
import Language.Haskell.Tools.AST.Representation.Decls as Exported
import Language.Haskell.Tools.AST.Representation.Binds as Exported
import Language.Haskell.Tools.AST.Representation.Exprs as Exported
import Language.Haskell.Tools.AST.Representation.Stmts as Exported
import Language.Haskell.Tools.AST.Representation.Patterns as Exported
import Language.Haskell.Tools.AST.Representation.Types as Exported
import Language.Haskell.Tools.AST.Representation.Kinds as Exported
import Language.Haskell.Tools.AST.Representation.Literals as Exported
import Language.Haskell.Tools.AST.Representation.Names as Exported
import Language.Haskell.Tools.AST.Ann as Exported
import Language.Haskell.Tools.AST.Utils.OrdSrcSpan as Exported
import Language.Haskell.Tools.AST.SemaInfoClasses as Exported
-- import Language.Haskell.Tools.AST.SemaInfoTypes (NoSemanticInfo, ScopeInfo, NameInfo, ) as Exported