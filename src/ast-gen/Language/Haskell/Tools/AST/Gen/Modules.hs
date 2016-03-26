-- | Generation of Module-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Modules where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList specs = mkAnn ("(" <> child <> ")") 
                               (ImportSpecList (AnnList (fromTemplate list) specs))

mkIeSpec :: TemplateAnnot a => Ann Name a -> AnnMaybe SubSpec a -> Ann IESpec a
mkIeSpec name ss = mkAnn (child <> child) (IESpec name ss)
        
mkSubList :: TemplateAnnot a => [Ann Name a] -> Ann SubSpec a
mkSubList names = mkAnn ("(" <> child <> ")") 
                        (SubSpecList (AnnList (fromTemplate list) names))

mkUnqualName' :: TemplateAnnot a => GHC.Name -> Ann Name a
mkUnqualName' = mkUnqualName . GHC.occNameString . GHC.getOccName
                      
mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = mkAnn (child <> child) 
                       (Name emptyList (Ann (fromTemplate (fromString n)) 
                                            (SimpleName n)))
    
-- decls 

mkTypeSignature :: TemplateAnnot a => Ann Name a -> Ann Type a -> Ann TypeSignature a
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (TypeSignature n t)
    
-- types    

mkTyForall :: TemplateAnnot a => AnnList TyVar a -> AnnMaybe Context a -> Ann Type a -> Ann Type a
mkTyForall vars ctx t = mkAnn ("forall " <> child <> " ." <> child <> " " <> child) (TyForall vars ctx t)

mkTyFun :: TemplateAnnot a => Ann Type a -> Ann Type a -> Ann Type a
mkTyFun at rt = mkAnn (child <> " -> " <> child) (TyFun at rt)

mkTyApp :: TemplateAnnot a => Ann Type a -> Ann Type a -> Ann Type a
mkTyApp ft at = mkAnn (child <> " " <> child) (TyApp ft at)
             
mkTypeVarList :: TemplateAnnot a => [GHC.Name] -> AnnList TyVar a
mkTypeVarList ls = mkAnnList (listSep " ") (map (mkTypeVar . mkUnqualName') ls)
           
mkTypeVar' :: TemplateAnnot a => GHC.Name -> Ann TyVar a
mkTypeVar' = mkTypeVar . mkUnqualName'
           
mkTypeVar :: TemplateAnnot a => Ann Name a -> Ann TyVar a
mkTypeVar n = mkAnn (child <> optBefore " ") (TyVarDecl n noth)
           
mkTypeVarType' :: TemplateAnnot a => GHC.Name -> Ann Type a
mkTypeVarType' = mkTypeVarType . mkUnqualName'

mkTypeVarType :: TemplateAnnot a => Ann Name a -> Ann Type a
mkTypeVarType n = mkAnn child (TyVar n)
              