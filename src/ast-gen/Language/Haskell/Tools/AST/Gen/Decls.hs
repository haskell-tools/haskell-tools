-- | Generation of Module-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Decls where

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


mkTypeSignature :: TemplateAnnot a => Ann Name a -> Ann Type a -> Ann TypeSignature a
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (TypeSignature (mkAnnList (listSep ", ") [n]) t)