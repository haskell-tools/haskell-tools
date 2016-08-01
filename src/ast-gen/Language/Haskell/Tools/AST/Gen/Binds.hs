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
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkSimpleBind :: Ann Pattern dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann ValueBind dom SrcTemplateStage
mkSimpleBind p r l = mkAnn (child <> child <> child) (SimpleBind p r (mkAnnMaybe (optBefore " ") l))

mkFunctionBind :: [Ann Match dom SrcTemplateStage] -> Ann ValueBind dom SrcTemplateStage
mkFunctionBind = mkAnn child . FunBind . mkAnnList indentedList

mkUnguardedRhs :: Ann Expr dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage
mkUnguardedRhs = mkAnn (" = " <> child) . UnguardedRhs

mkMatch :: Ann MatchLhs dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann Match dom SrcTemplateStage
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ Match lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkNormalMatchLhs :: Ann Name dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann MatchLhs dom SrcTemplateStage
mkNormalMatchLhs n pats = mkAnn (child <> child) $ NormalLhs n (mkAnnList (listSepBefore " " " ") pats)

mkLocalValBind :: Ann ValueBind dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalValBind = mkAnn child . LocalValBind

mkLocalBinds :: Int -> [Ann LocalBind dom SrcTemplateStage] -> AnnMaybe LocalBinds dom SrcTemplateStage
mkLocalBinds col = mkAnnMaybe (optBefore ("\n" ++ replicate (col - 1) ' ' ++ "where ")) 
                     . Just . mkAnn child . LocalBinds . mkAnnList indentedList