-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Stmts where

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

mkBindStmt :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
mkBindStmt bound expr = mkAnn (child <> " <- " <> child) $ BindStmt bound expr

mkExprStmt :: Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
mkExprStmt = mkAnn child . ExprStmt

mkLetStmt :: [Ann LocalBind dom SrcTemplateStage] -> Ann Stmt dom SrcTemplateStage
mkLetStmt = mkAnn ("let " <> child) . LetStmt . mkAnnList indentedList

mkListCompBody :: [Ann CompStmt dom SrcTemplateStage] -> Ann ListCompBody dom SrcTemplateStage
mkListCompBody = mkAnn child . ListCompBody . mkAnnList (listSep " ")

mkCompStmt :: Ann Stmt dom SrcTemplateStage -> Ann CompStmt dom SrcTemplateStage
mkCompStmt = mkAnn child . CompStmt
