-- | The FromGHC module provides a way to transform the GHC AST into our AST. This transformation is done in
-- the Ghc monad. The conversion can be performed from the Parsed and the Renamed GHC AST. If the renamed AST
-- is given, additional semantic information is looked up while traversing the AST.
module Language.Haskell.Tools.BackendGHC
  ( trfModule, trfModuleRename, addTypeInfos, runTrf
  , SpliceInsertionProblem(..), ConvertionProblem(..)
  ) where

import Language.Haskell.Tools.BackendGHC.AddTypeInfo (addTypeInfos)
import Language.Haskell.Tools.BackendGHC.Modules (trfModule, trfModuleRename)
import Language.Haskell.Tools.BackendGHC.Monad (runTrf, SpliceInsertionProblem(..))
import Language.Haskell.Tools.BackendGHC.Utils (ConvertionProblem(..))
