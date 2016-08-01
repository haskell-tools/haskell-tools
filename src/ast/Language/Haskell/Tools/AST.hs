-- | A custom AST representation for Haskell tools.
-- Different layers of the AST are recursive, to separate them into modules
-- we introduced source imports.
module Language.Haskell.Tools.AST 
  ( module Exported
  ) where

import Language.Haskell.Tools.AST.Instances

import Language.Haskell.Tools.AST.References as Exported
import Language.Haskell.Tools.AST.Helpers as Exported

import Language.Haskell.Tools.AST.Modules as Exported
import Language.Haskell.Tools.AST.TH as Exported
import Language.Haskell.Tools.AST.Decls as Exported
import Language.Haskell.Tools.AST.Binds as Exported
import Language.Haskell.Tools.AST.Exprs as Exported
import Language.Haskell.Tools.AST.Stmts as Exported
import Language.Haskell.Tools.AST.Patterns as Exported
import Language.Haskell.Tools.AST.Types as Exported
import Language.Haskell.Tools.AST.Kinds as Exported
import Language.Haskell.Tools.AST.Literals as Exported
import Language.Haskell.Tools.AST.Base as Exported
import Language.Haskell.Tools.AST.Ann as Exported
import Language.Haskell.Tools.AST.Utils.OrdSrcSpan as Exported
import Language.Haskell.Tools.AST.SemaInfoTypes as Exported