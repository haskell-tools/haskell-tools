-- | Generation of binding-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
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

mkSimpleBind :: TemplateAnnot a => Ann Pattern a -> Ann Rhs a -> Maybe (Ann LocalBinds a) -> Ann ValueBind a
mkSimpleBind p r l = mkAnn (child <> child <> child) (SimpleBind p r (mkAnnMaybe (optBefore " ") l))

mkFunctionBind :: TemplateAnnot a => [Ann Match a] -> Ann ValueBind a
mkFunctionBind = mkAnn child . FunBind . mkAnnList indentedList

mkUnguardedRhs :: TemplateAnnot a => Ann Expr a -> Ann Rhs a
mkUnguardedRhs = mkAnn (" = " <> child) . UnguardedRhs

mkMatch :: TemplateAnnot a => Ann MatchLhs a -> Ann Rhs a -> Maybe (Ann LocalBinds a) -> Ann Match a
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ Match lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkNormalMatchLhs :: TemplateAnnot a => Ann Name a -> [Ann Pattern a] -> Ann MatchLhs a
mkNormalMatchLhs n pats = mkAnn (child <> child) $ NormalLhs n (mkAnnList (listSepBefore " " " ") pats)
