-- | Public interface for the modules that can be used to pattern match on the AST.
module Language.Haskell.Tools.AST.Match 
  ( module Language.Haskell.Tools.AST.Match.Modules
  , module Language.Haskell.Tools.AST.Match.Decls
  , module Language.Haskell.Tools.AST.Match.Binds
  , module Language.Haskell.Tools.AST.Match.Types
  , module Language.Haskell.Tools.AST.Match.Kinds
  , module Language.Haskell.Tools.AST.Match.Exprs
  , module Language.Haskell.Tools.AST.Match.Stmts
  , module Language.Haskell.Tools.AST.Match.Literals
  , module Language.Haskell.Tools.AST.Match.Patterns
  , module Language.Haskell.Tools.AST.Match.Names
  , module Language.Haskell.Tools.AST.Match.TH
  ) where

import Language.Haskell.Tools.AST.Match.Binds
import Language.Haskell.Tools.AST.Match.Decls
import Language.Haskell.Tools.AST.Match.Exprs
import Language.Haskell.Tools.AST.Match.Kinds
import Language.Haskell.Tools.AST.Match.Literals
import Language.Haskell.Tools.AST.Match.Modules
import Language.Haskell.Tools.AST.Match.Names
import Language.Haskell.Tools.AST.Match.Patterns
import Language.Haskell.Tools.AST.Match.Stmts
import Language.Haskell.Tools.AST.Match.TH
import Language.Haskell.Tools.AST.Match.Types
