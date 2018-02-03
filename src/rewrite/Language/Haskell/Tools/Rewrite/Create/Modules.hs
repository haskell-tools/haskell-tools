-- | Generation of UModule-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkModule@ creates the annotated version of the @UModule@ AST constructor.
{-# LANGUAGE ExplicitNamespaces, KindSignatures, MonoLocalBinds, OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Modules where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Names (mkStringNode)
import Language.Haskell.Tools.Rewrite.Create.Utils
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
mkModule :: [FilePragma] -> Maybe ModuleHead -> [ImportDecl] -> [Decl] -> Module
mkModule filePrags head imps decls
  = mkAnn (child <> child <> child <> child)
      $ UModule (mkAnnList (followedBy "\n" $ separatedBy "\n" list) filePrags) (mkAnnMaybe opt head)
                (mkAnnList (after "\n" $ indented list) imps) (mkAnnList (after "\n" $ indented list) decls)

-- | Module declaration with name and (optional) exports
mkModuleHead :: ModuleName -> Maybe ModulePragma -> Maybe ExportSpecs -> ModuleHead
mkModuleHead n pr es = mkAnn ("module " <> child <> child <> child <> " where")
                         $ UModuleHead n (mkAnnMaybe (after "\n" opt) pr) (mkAnnMaybe opt es)

-- | A list of export specifications surrounded by parentheses
mkExportSpecs :: [ExportSpec] -> ExportSpecs
mkExportSpecs = mkAnn ("(" <> child <> ")") . UExportSpecs . mkAnnList (separatedBy ", " list)

-- | Export a name and related names
mkExportSpec :: IESpec -> ExportSpec
mkExportSpec = mkAnn child . UDeclExport

-- | The export of an imported module (@ module A @)
mkModuleExport :: ModuleName -> ExportSpec
mkModuleExport = mkAnn ("module " <> child) . UModuleExport

-- | Marks a name to be imported or exported with related names (subspecifier)
mkIESpec :: Name -> Maybe SubSpec -> IESpec
mkIESpec name ss = mkAnn (child <> child <> child) (UIESpec noth name (mkAnnMaybe (after "(" $ followedBy ")" opt) ss))

-- | Marks a pattern synonym to be imported or exported
mkPatternIESpec :: Name -> IESpec
mkPatternIESpec name = mkAnn (child <> child) (UIESpec (justVal $ mkAnn child UImportPattern) name noth)

-- | @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
mkSubList :: [Name] -> SubSpec
mkSubList = mkAnn child . USubSpecList . mkAnnList (separatedBy ", " list)

-- | @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
mkSubAll :: SubSpec
mkSubAll = mkAnn ".." USubSpecAll

-- | An import declaration: @import Module.Name@
mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> ModuleName -> Maybe ModuleName -> Maybe ImportSpec
                  -> ImportDecl
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      UImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " UImportSource) else noth)
                  (if qualified then justVal (mkAnn "qualified " UImportQualified) else noth)
                  (if safe then justVal (mkAnn "safe " UImportSafe) else noth)
                  (case pkg of Just str -> justVal (mkStringNode str); _ -> noth)
                  name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . UImportRenaming) rename)) (mkAnnMaybe opt spec)

-- | Restrict the import definition to ONLY import the listed names
mkImportSpecList :: [IESpec] -> ImportSpec
mkImportSpecList = mkAnn ("(" <> child <> ")") . UImportSpecList . mkAnnList (separatedBy ", " list)

-- | Restrict the import definition to DONT import the listed names
mkImportHidingList :: [IESpec] -> ImportSpec
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . UImportSpecHiding . mkAnnList (separatedBy ", " list)

-- | The name of a module
mkModuleName :: String -> ModuleName
mkModuleName s = mkAnn (fromString s) (UModuleName s)

-- * Pragmas

mkFilePragmas :: [FilePragma] -> FilePragmaList
mkFilePragmas = mkAnnList (separatedBy "\n" list)

-- | @LANGUAGE@ pragma, listing the enabled language extensions in that file
mkLanguagePragma :: [String] -> FilePragma
mkLanguagePragma extensions
  = mkAnn ("{-# LANGUAGE " <> child <> " #-}") $ ULanguagePragma
      $ mkAnnList (separatedBy ", " list) (map (\ext -> mkAnn (fromString ext) (ULanguageExtension ext)) extensions)

-- | @OPTIONS@ pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
mkOptionsGHC :: String -> FilePragma
mkOptionsGHC opts
  = mkAnn ("{-# OPTIONS_GHC " <> child <> " #-}") $ UOptionsPragma
      $ mkStringNode opts

-- | A warning pragma attached to the module
mkModuleWarningPragma :: [String] -> ModulePragma
mkModuleWarningPragma msg
  = mkAnn ("{-# WARNING " <> child <> " #-}") $ UModuleWarningPragma
      $ mkAnnList (separatedBy " " list) $ map mkStringNode msg

-- | A deprecated pragma attached to the module
mkModuleDeprecatedPragma :: [String] -> ModulePragma
mkModuleDeprecatedPragma msg
  = mkAnn ("{-# DEPRECATED " <> child <> " #-}") $ UModuleDeprecatedPragma
      $ mkAnnList (separatedBy " " list) $ map mkStringNode msg
