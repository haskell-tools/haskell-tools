-- | Generation of expression-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkApp@ creates the annotated version of the @App@ AST constructor.
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

mkParen :: TemplateAnnot a => Ann Expr a -> Ann Expr a
mkParen = mkAnn ("(" <> child <> ")") . Paren
