-- | Generation of UModule-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkModule@ creates the annotated version of the @UModule@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Modules where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.Transform

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
mkModule :: [FilePragma dom] -> Maybe (ModuleHead dom) -> [ImportDecl dom] -> [Decl dom] -> Module dom
mkModule filePrags head imps decls 
  = mkAnn (child <> child <> child <> child) 
      $ UModule (mkAnnList (listSepAfter "\n" "\n") filePrags) (mkAnnMaybe opt head)
                (mkAnnList (indentedListBefore "\n") imps) (mkAnnList (indentedListBefore "\n") decls)
               
-- | Module declaration with name and (optional) exports
mkModuleHead :: ModuleName dom -> Maybe (ExportSpecs dom) -> Maybe (ModulePragma dom) -> ModuleHead dom
mkModuleHead n es pr = mkAnn ("module " <> child <> child <> child <> " where") $ UModuleHead n (mkAnnMaybe opt es) (mkAnnMaybe (optBefore "\n") pr)

-- | A list of export specifications surrounded by parentheses
mkExportSpecs :: [ExportSpec dom] -> ExportSpecs dom
mkExportSpecs = mkAnn ("(" <> child <> ")") . UExportSpecs . mkAnnList (listSep ", ")

-- | Export a name and related names
mkExportSpec :: IESpec dom -> ExportSpec dom
mkExportSpec = mkAnn child . UDeclExport

-- | The export of an imported module (@ module A @)
mkModuleExport :: ModuleName dom -> ExportSpec dom
mkModuleExport = mkAnn ("module " <> child) . UModuleExport

-- | Marks a name to be imported or exported with related names (subspecifier)
mkIESpec :: Name dom -> Maybe (SubSpec dom) -> IESpec dom
mkIESpec name ss = mkAnn (child <> child) (UIESpec name (mkAnnMaybe opt ss))

-- | @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
mkSubList :: [Name dom] -> SubSpec dom
mkSubList = mkAnn ("(" <> child <> ")") . USubSpecList . mkAnnList (listSep ", ")

-- | @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
mkSubAll :: SubSpec dom
mkSubAll = mkAnn "(..)" USubSpecAll

-- | An import declaration: @import Module.Name@         
mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> ModuleName dom -> Maybe (ModuleName dom) -> Maybe (ImportSpec dom) 
                  -> ImportDecl dom       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      UImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " UImportSource) else noth)
                  (if qualified then justVal (mkAnn "qualified " UImportQualified) else noth)
                  (if safe then justVal (mkAnn "safe " UImportSafe) else noth)
                  (case pkg of Just str -> justVal (mkStringNode str); _ -> noth)
                  name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . UImportRenaming) rename)) (mkAnnMaybe opt spec)

-- | Restrict the import definition to ONLY import the listed names
mkImportSpecList :: [IESpec dom] -> ImportSpec dom
mkImportSpecList = mkAnn ("(" <> child <> ")") . UImportSpecList . mkAnnList (listSep ", ")

-- | Restrict the import definition to DONT import the listed names
mkImportHidingList :: [IESpec dom] -> ImportSpec dom
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . UImportSpecHiding . mkAnnList (listSep ", ")

-- | The name of a module
mkModuleName :: String -> ModuleName dom
mkModuleName s = mkAnn (fromString s) (UModuleName s)

-- * Pragmas

-- | @LANGUAGE@ pragma, listing the enabled language extensions in that file
mkLanguagePragma :: [String] -> FilePragma dom
mkLanguagePragma extensions 
  = mkAnn ("{-# LANGUAGE " <> child <> " #-}") $ ULanguagePragma 
      $ mkAnnList (listSep ", ") (map (\ext -> mkAnn (fromString ext) (ULanguageExtension ext)) extensions)

-- | @OPTIONS@ pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
mkOptionsGHC :: String -> FilePragma dom
mkOptionsGHC opts 
  = mkAnn ("{-# OPTIONS_GHC " <> child <> " #-}") $ UOptionsPragma 
      $ mkStringNode opts

-- | A warning pragma attached to the module
mkModuleWarningPragma :: [String] -> ModulePragma dom
mkModuleWarningPragma msg 
  = mkAnn ("{-# WARNING " <> child <> " #-}") $ UModuleWarningPragma 
      $ mkAnnList (listSep " ") $ map mkStringNode msg

-- | A deprecated pragma attached to the module
mkModuleDeprecatedPragma :: [String] -> ModulePragma dom
mkModuleDeprecatedPragma msg 
  = mkAnn ("{-# DEPRECATED " <> child <> " #-}") $ UModuleDeprecatedPragma 
      $ mkAnnList (listSep " ") $ map mkStringNode msg
