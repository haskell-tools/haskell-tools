-- | Generation of binding-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkMatch@ creates the annotated version of the @Match@ constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Binds where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Patterns
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkSimpleBind' :: Ann Name dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann ValueBind dom SrcTemplateStage
mkSimpleBind' n e = mkSimpleBind (mkVarPat n) (mkUnguardedRhs e) Nothing

mkSimpleBind :: Ann Pattern dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann ValueBind dom SrcTemplateStage
mkSimpleBind p r l = mkAnn (child <> child <> child) (USimpleBind p r (mkAnnMaybe opt l))

mkFunctionBind :: [Ann Match dom SrcTemplateStage] -> Ann ValueBind dom SrcTemplateStage
mkFunctionBind = mkAnn child . UFunBind . mkAnnList indentedList

mkFunctionBind' :: Ann Name dom SrcTemplateStage -> [([Ann Pattern dom SrcTemplateStage], Ann Expr dom SrcTemplateStage)] -> Ann ValueBind dom SrcTemplateStage
mkFunctionBind' name matches = mkFunctionBind $ map (\(args, rhs) -> mkMatch (mkMatchLhs name args) (mkUnguardedRhs rhs) Nothing) matches

mkMatch :: Ann MatchLhs dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann Match dom SrcTemplateStage
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ UMatch lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkMatchLhs :: Ann Name dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann MatchLhs dom SrcTemplateStage
mkMatchLhs n pats = mkAnn (child <> child) $ UNormalLhs n (mkAnnList (listSepBefore " " " ") pats)

mkInfixLhs :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage 
                -> [Ann Pattern dom SrcTemplateStage] -> Ann MatchLhs dom SrcTemplateStage
mkInfixLhs lhs op rhs pats = mkAnn (child <> child <> child <> child) $ UInfixLhs lhs op rhs (mkAnnList (listSepBefore " " " ") pats)

mkLocalBinds :: Int -> [Ann LocalBind dom SrcTemplateStage] -> AnnMaybe LocalBinds dom SrcTemplateStage
mkLocalBinds col = mkAnnMaybe (optBefore ("\n" ++ replicate (col - 1) ' ' ++ "where ")) 
                     . Just . mkAnn child . ULocalBinds . mkAnnList indentedList

mkLocalBinds' :: [Ann LocalBind dom SrcTemplateStage] -> Ann LocalBinds dom SrcTemplateStage
mkLocalBinds' = mkAnn (" where " <> child) . ULocalBinds . mkAnnList indentedList

mkLocalValBind :: Ann ValueBind dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalValBind = mkAnn child . ULocalValBind

mkLocalTypeSig :: Ann TypeSignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalTypeSig = mkAnn child . ULocalSignature

mkLocalFixity :: Ann FixitySignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalFixity = mkAnn child . ULocalFixity

mkTypeSignature :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann TypeSignature dom SrcTemplateStage
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (UTypeSignature (mkAnnList (listSep ", ") [n]) t)

mkInfixL :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixl" AssocLeft) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfixR :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixr" AssocRight) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfix :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child) 
                    $ UFixitySignature (mkAnn "infix" AssocNone) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkUnguardedRhs :: Ann Expr dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage
mkUnguardedRhs = mkAnn (" = " <> child) . UUnguardedRhs

mkGuardedRhss :: [Ann GuardedRhs dom SrcTemplateStage] -> Ann Rhs dom SrcTemplateStage
mkGuardedRhss = mkAnn child . UGuardedRhss . mkAnnList indentedList

mkGuardedRhs :: [Ann RhsGuard dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann GuardedRhs dom SrcTemplateStage
mkGuardedRhs guards expr = mkAnn ("| " <> child <> " = " <> child) $ UGuardedRhs (mkAnnList (listSep ", ") guards) expr

mkGuardBind :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ UGuardBind pat expr

mkGuardLet :: [Ann LocalBind dom SrcTemplateStage] -> Ann RhsGuard dom SrcTemplateStage
mkGuardLet = mkAnn ("let " <> child) . UGuardLet . mkAnnList indentedList

mkGuardCheck :: Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
mkGuardCheck = mkAnn child . UGuardCheck

-- pragmas are omitted
