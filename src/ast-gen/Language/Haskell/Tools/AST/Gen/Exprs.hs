-- | Generation of expression-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Exprs where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkApp :: TemplateAnnot a => Ann Expr a -> Ann Expr a -> Ann Expr a
mkApp f e = mkAnn (child <> " " <> child) (App f e)

mkVar :: TemplateAnnot a => Ann Name a -> Ann Expr a
mkVar = mkAnn child . Var
