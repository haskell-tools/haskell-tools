-- | A custom AST representation for Haskell tools.
-- Different layers of the AST are recursive, to separate them into modules
-- we introduced source imports.
module Language.Haskell.Tools.AST 
  ( module Language.Haskell.Tools.AST.References
  , module Language.Haskell.Tools.AST.Helpers
  , module Language.Haskell.Tools.AST.Representation.Modules
  , module Language.Haskell.Tools.AST.Representation.TH
  , module Language.Haskell.Tools.AST.Representation.Decls
  , module Language.Haskell.Tools.AST.Representation.Binds
  , module Language.Haskell.Tools.AST.Representation.Exprs
  , module Language.Haskell.Tools.AST.Representation.Stmts
  , module Language.Haskell.Tools.AST.Representation.Patterns
  , module Language.Haskell.Tools.AST.Representation.Types
  , module Language.Haskell.Tools.AST.Representation.Kinds
  , module Language.Haskell.Tools.AST.Representation.Literals
  , module Language.Haskell.Tools.AST.Representation.Names
  , module Language.Haskell.Tools.AST.Ann
  , module Language.Haskell.Tools.AST.Utils.OrdSrcSpan
  , module Language.Haskell.Tools.AST.SemaInfoClasses
  ) where

import Language.Haskell.Tools.AST.Instances ()

import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Utils.OrdSrcSpan
import Language.Haskell.Tools.AST.SemaInfoClasses
