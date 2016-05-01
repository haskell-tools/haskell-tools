-- | Generation of pattern-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Patterns where

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

mkVarPat :: TemplateAnnot a => Ann Name a -> Ann Pattern a
mkVarPat = mkAnn child . VarPat

mkAppPat :: TemplateAnnot a => Ann Name a -> [Ann Pattern a] -> Ann Pattern a
mkAppPat n pat = mkAnn (child <> child) $ AppPat n (mkAnnList (listSepBefore " " " ") pat)