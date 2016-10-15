-- | Generation of expression-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkApp@ creates the annotated version of the @App@ AST constructor.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies
           #-}
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

-- * Expressions

mkVar :: Ann Name dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkVar = mkAnn child . UVar

mkLit :: Ann Literal dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkLit = mkAnn child . ULit

mkInfixApp :: Ann Expr dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkInfixApp lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixApp lhs op rhs

mkPrefixApp :: Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkPrefixApp op rhs = mkAnn (child <> child) $ UPrefixApp op rhs

mkApp :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkApp f e = mkAnn (child <> " " <> child) (UApp f e)

mkLambda :: [Ann Pattern dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkLambda pats rhs = mkAnn ("\\" <> child <> " -> " <> child) $ ULambda (mkAnnList (listSep " ") pats) rhs

mkLet :: [Ann LocalBind dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkLet pats expr = mkAnn ("let " <> child <> " in " <> child) $ ULet (mkAnnList indentedList pats) expr

mkIf :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkIf cond then_ else_ = mkAnn ("if " <> child <> " then " <> child <> " else " <> child) $ UIf cond then_ else_

mkMultiIf :: [Ann GuardedCaseRhs dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkMultiIf cases = mkAnn ("if" <> child) $ UMultiIf (mkAnnList indentedList cases)

mkCase :: Ann Expr dom SrcTemplateStage -> [Ann Alt dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkCase expr cases = mkAnn ("case " <> child <> " of " <> child) $ UCase expr (mkAnnList indentedList cases)

mkDoBlock :: [Ann Stmt dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkDoBlock stmts = mkAnn (child <> " " <> child) $ UDo (mkAnn "do" UDoKeyword) (mkAnnList indentedList stmts)

mkTuple :: [Ann Expr dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkTuple exprs = mkAnn ("(" <> child <> ")") $ UTuple (mkAnnList (listSep ", ") exprs)

mkUnboxedTuple :: [Ann Expr dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkUnboxedTuple exprs = mkAnn ("(# " <> child <> " #)") $ UTuple (mkAnnList (listSep ", ") exprs)

mkList :: [Ann Expr dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkList exprs = mkAnn ("[" <> child <> "]") $ UList (mkAnnList (listSep ", ") exprs)

mkParen :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkParen = mkAnn ("(" <> child <> ")") . UParen

mkLeftSection :: Ann Expr dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkLeftSection lhs op = mkAnn ("(" <> child <> child <> ")") $ ULeftSection lhs op

mkRightSection :: Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkRightSection op rhs = mkAnn ("(" <> child <> child <> ")") $ URightSection op rhs

mkRecCon :: Ann Name dom SrcTemplateStage -> [Ann FieldUpdate dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkRecCon name flds = mkAnn (child <> " { " <> child <> " }") $ URecCon name (mkAnnList (listSep ", ") flds)

mkRecUpdate :: Ann Expr dom SrcTemplateStage -> [Ann FieldUpdate dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage
mkRecUpdate expr flds = mkAnn (child <> " { " <> child <> " }") $ URecUpdate expr (mkAnnList (listSep ", ") flds)

mkEnum :: Ann Expr dom SrcTemplateStage -> Maybe (Ann Expr dom SrcTemplateStage) -> Maybe (Ann Expr dom SrcTemplateStage) -> Ann Expr dom SrcTemplateStage
mkEnum from step to = mkAnn ("[" <> child <> child <> ".." <> child <> "]") $ UEnum from (mkAnnMaybe (optBefore ",") step) (mkAnnMaybe (optBefore ",") to)

mkExprTypeSig :: Ann Expr dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
mkExprTypeSig lhs typ = mkAnn (child <> " :: " <> child) $ UExplTypeApp lhs typ

-- * Field updates

mkFieldUpdate :: Ann Name dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
mkFieldUpdate name val = mkAnn (child <> " = " <> child) $ UNormalFieldUpdate name val

mkFieldPun :: Ann Name dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
mkFieldPun name = mkAnn child $ UFieldPun name

mkFieldWildcard :: Ann FieldWildcard dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
mkFieldWildcard name = mkAnn child $ UFieldWildcard name


-- * Pattern matching and guards

mkAlt :: Ann Pattern dom SrcTemplateStage -> Ann CaseRhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann Alt dom SrcTemplateStage
mkAlt pat rhs locals = mkAnn (child <> child <> child) $ UAlt pat rhs (mkAnnMaybe (optBefore " where ") locals)

mkCaseRhs :: Ann Expr dom SrcTemplateStage -> Ann CaseRhs dom SrcTemplateStage
mkCaseRhs = mkAnn (" -> " <> child) . UUnguardedCaseRhs

mkGuardedCaseRhss :: [Ann GuardedCaseRhs dom SrcTemplateStage] -> Ann CaseRhs dom SrcTemplateStage
mkGuardedCaseRhss = mkAnn child . UGuardedCaseRhss . mkAnnList indentedList

mkGuardedCaseRhs :: [Ann RhsGuard dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann GuardedCaseRhs dom SrcTemplateStage
mkGuardedCaseRhs guards expr = mkAnn (" | " <> child <> " -> " <> child) $ UGuardedCaseRhs (mkAnnList (listSep ", ") guards) expr


