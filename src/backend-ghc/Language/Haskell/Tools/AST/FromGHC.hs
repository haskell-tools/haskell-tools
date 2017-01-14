-- | The FromGHC module provides a way to transform the GHC AST into our AST. This transformation is done in 
-- the Ghc monad. The conversion can be performed from the Parsed and the Renamed GHC AST. If the renamed AST 
-- is given, additional semantic information is looked up while traversing the AST. 
module Language.Haskell.Tools.AST.FromGHC 
  ( trfModule, trfModuleRename, addTypeInfos, runTrf ) where

import Language.Haskell.Tools.AST.FromGHC.AddTypeInfo (addTypeInfos)
import Language.Haskell.Tools.AST.FromGHC.Modules (trfModule, trfModuleRename)
import Language.Haskell.Tools.AST.FromGHC.Monad (runTrf)
