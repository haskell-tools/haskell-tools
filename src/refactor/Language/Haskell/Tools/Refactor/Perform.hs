{-# LANGUAGE StandaloneDeriving
           , DeriveGeneric
           , LambdaCase
           , ScopedTypeVariables
           , BangPatterns
           , MultiWayIf
           , FlexibleContexts
           , TypeFamilies
           , TupleSections
           , TemplateHaskell
           , ViewPatterns
           #-}
-- | Defines common utilities for using refactorings. Provides an interface for both demo, command line and integrated tools.
module Language.Haskell.Tools.Refactor.Perform where

import Data.List.Split

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.Predefined.FloatOut
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.InlineBinding
import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase

import GHC

-- | Executes a given command on the selected module and given other modules
performCommand :: (HasModuleInfo dom, DomGenerateExports dom, OrganizeImportsDomain dom, DomainRenameDefinition dom, ExtractBindingDomain dom, GenerateSignatureDomain dom)
               => RefactorCommand -> ModuleDom dom -- ^ The module in which the refactoring is performed
                                  -> [ModuleDom dom] -- ^ Other modules
                                  -> Ghc (Either String [RefactorChange dom])
performCommand rf mod mods = runRefactor mod mods $ selectCommand rf
  where selectCommand NoRefactor = localRefactoring return
        selectCommand OrganizeImports = localRefactoring organizeImports
        selectCommand ProjectOrganizeImports = projectOrganizeImports
        selectCommand GenerateExports = localRefactoring generateExports
        selectCommand (GenerateSignature sp) = localRefactoring $ generateTypeSignature' (correctRefactorSpan (snd mod) sp)
        selectCommand (RenameDefinition sp str) = renameDefinition' (correctRefactorSpan (snd mod) sp) str
        selectCommand (ExtractBinding sp str) = localRefactoring $ extractBinding' (correctRefactorSpan (snd mod) sp) str
        selectCommand (InlineBinding sp) = inlineBinding (correctRefactorSpan (snd mod) sp)
        selectCommand (FloatOut sp) = localRefactoring $ floatOut (correctRefactorSpan (snd mod) sp)

-- | A refactoring command
data RefactorCommand = NoRefactor
                     | OrganizeImports
                     | ProjectOrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
                     | InlineBinding RealSrcSpan
                     | FloatOut RealSrcSpan
    deriving Show

-- | Recognize a command from its textual representation
readCommand :: String -> Either String RefactorCommand
readCommand (splitOn " " -> refact:args) = analyzeCommand refact args
readCommand _ = error "panic: splitOn resulted empty"

-- | Check the parts and return the command
analyzeCommand :: String -> [String] -> Either String RefactorCommand
analyzeCommand "" _ = Right NoRefactor
analyzeCommand "CheckSource" _ = Right NoRefactor
analyzeCommand "OrganizeImports" _ = Right OrganizeImports
analyzeCommand "ProjectOrganizeImports" _ = Right ProjectOrganizeImports
analyzeCommand "GenerateExports" _ = Right GenerateExports
analyzeCommand "GenerateSignature" [sp] = Right $ GenerateSignature (readSrcSpan sp)
analyzeCommand "GenerateSignature" _ = Left $ "GenerateSignature needs one argument: the source range of the definition"
analyzeCommand "RenameDefinition" [sp, newName] = Right $ RenameDefinition (readSrcSpan sp) newName
analyzeCommand "RenameDefinition" _ = Left $ "RenameDefinition needs two arguments: the source range of the name and the new name"
analyzeCommand "ExtractBinding" [sp, newName] = Right $ ExtractBinding (readSrcSpan sp) newName
analyzeCommand "ExtractBinding" _ = Left $ "RenameDefinition needs two arguments: the source range of the expression and the new name"
analyzeCommand "InlineBinding" [sp] = Right $ InlineBinding (readSrcSpan sp)
analyzeCommand "InlineBinding" _ = Left $ "InlineBinding needs one argument: the source range of the definition"
analyzeCommand "FloatOut" [sp] = Right $ FloatOut (readSrcSpan sp)
analyzeCommand "FloatOut" _ = Left $ "FloatOut needs one argument: the source range of the definition"
analyzeCommand ref _ = Left $ "Unknown command: " ++ ref

refactorCommands :: [String]
refactorCommands
  = [ "CheckSource", "OrganizeImports", "ProjectOrganizeImports", "GenerateExports"
    , "GenerateSignature", "RenameDefinition", "ExtractBinding", "InlineBinding"
    , "FloatOut" ]
