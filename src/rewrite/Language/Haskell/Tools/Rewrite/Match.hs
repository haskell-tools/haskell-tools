-- | Public interface for the modules that can be used to pattern match on the AST.
module Language.Haskell.Tools.Rewrite.Match
  ( module Language.Haskell.Tools.Rewrite.Match.Modules
  , module Language.Haskell.Tools.Rewrite.Match.Decls
  , module Language.Haskell.Tools.Rewrite.Match.Binds
  , module Language.Haskell.Tools.Rewrite.Match.Types
  , module Language.Haskell.Tools.Rewrite.Match.Kinds
  , module Language.Haskell.Tools.Rewrite.Match.Exprs
  , module Language.Haskell.Tools.Rewrite.Match.Stmts
  , module Language.Haskell.Tools.Rewrite.Match.Literals
  , module Language.Haskell.Tools.Rewrite.Match.Patterns
  , module Language.Haskell.Tools.Rewrite.Match.Names
  , module Language.Haskell.Tools.Rewrite.Match.TH
  ) where

import Language.Haskell.Tools.Rewrite.Match.Binds
import Language.Haskell.Tools.Rewrite.Match.Decls
import Language.Haskell.Tools.Rewrite.Match.Exprs
import Language.Haskell.Tools.Rewrite.Match.Kinds
import Language.Haskell.Tools.Rewrite.Match.Literals
import Language.Haskell.Tools.Rewrite.Match.Modules
import Language.Haskell.Tools.Rewrite.Match.Names
import Language.Haskell.Tools.Rewrite.Match.Patterns
import Language.Haskell.Tools.Rewrite.Match.Stmts
import Language.Haskell.Tools.Rewrite.Match.TH
import Language.Haskell.Tools.Rewrite.Match.Types
