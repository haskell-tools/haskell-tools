-- | Generation of declaration-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTypeSignature@ creates the annotated version of the @TypeSignature@ AST constructor.
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

mkValueBinding :: TemplateAnnot a => Ann ValueBind a -> Ann Decl a
mkValueBinding = mkAnn child . ValueBinding

