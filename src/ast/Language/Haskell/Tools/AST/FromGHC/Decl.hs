module Language.Haskell.Tools.AST.FromGHC.Decl where

import RdrName as GHC
import HsSyn as GHC

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Decl as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
trfDecls _ = pure $ AnnList []