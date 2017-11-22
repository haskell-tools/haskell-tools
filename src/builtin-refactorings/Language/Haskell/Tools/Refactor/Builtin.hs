{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Language.Haskell.Tools.Refactor.Builtin ( builtinRefactorings ) where

import Language.Haskell.Tools.Refactor (RefactoringChoice)
import Language.Haskell.Tools.Refactor.Builtin.ExtractBinding (ExtractBindingDomain, extractBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.FloatOut (floatOutRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateExports (DomGenerateExports, generateExportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateTypeSignature (GenerateSignatureDomain, generateTypeSignatureRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.InlineBinding (inlineBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions (OrganizeExtensionsDomain, organizeExtensionsRefactoring, projectOrganizeExtensionsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeImports (OrganizeImportsDomain, organizeImportsRefactoring, projectOrganizeImportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.RenameDefinition (DomainRenameDefinition, renameDefinitionRefactoring)

builtinRefactorings :: ( DomGenerateExports dom, OrganizeImportsDomain dom
                       , DomainRenameDefinition dom, ExtractBindingDomain dom
                       , GenerateSignatureDomain dom, OrganizeExtensionsDomain dom
                       ) => [RefactoringChoice dom]
builtinRefactorings
  = [ organizeImportsRefactoring
    , projectOrganizeImportsRefactoring
    , inlineBindingRefactoring
    , generateTypeSignatureRefactoring
    , renameDefinitionRefactoring
    , generateExportsRefactoring
    , floatOutRefactoring
    , extractBindingRefactoring
    , organizeExtensionsRefactoring
    , projectOrganizeExtensionsRefactoring
    ]
