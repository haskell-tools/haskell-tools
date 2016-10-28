-- | The FromGHC module provides a way to transform the GHC AST into our AST. This transformation is done in 
-- the Ghc monad. The conversion can be performed from the Parsed and the Renamed GHC AST. If the renamed AST 
-- is given, additional semantic information is looked up while traversing the AST. 
module Language.Haskell.Tools.AST.FromGHC 
  ( module Language.Haskell.Tools.AST.FromGHC.Modules
  , module Language.Haskell.Tools.AST.FromGHC.Monad
  , module Language.Haskell.Tools.AST.FromGHC.Names
  , module Language.Haskell.Tools.AST.FromGHC.Utils
  ) where

import Language.Haskell.Tools.AST.FromGHC.Modules
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Utils