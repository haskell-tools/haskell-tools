-- | Defines a representation to represent refactorings that can be executed on the codebase.
-- Refactorings are differentiated on their signatures (inputs needed to execute).
module Language.Haskell.Tools.Refactor.Refactoring where

import Control.Monad.Trans.Except (runExceptT)
import Data.List ((++), map, find)
import Data.Aeson()

import GHC (RealSrcSpan, Ghc)

import Language.Haskell.Tools.AST ()
import Language.Haskell.Tools.Refactor.Monad (ProjectRefactoring, Refactoring)
import Language.Haskell.Tools.Refactor.Prepare (correctRefactorSpan, readSrcSpan)
import Language.Haskell.Tools.Refactor.Representation (RefactorChange, ModuleDom)

-- | The signature and behavior of one refactoring that can be executed.
data RefactoringChoice
  = NamingRefactoring { refactoringName :: String
                      , namingRefactoring :: RealSrcSpan -> String -> Refactoring
                      }
  | SelectionRefactoring { refactoringName :: String
                         , selectionRefactoring :: RealSrcSpan -> Refactoring
                         }
  | ModuleRefactoring { refactoringName :: String
                      , moduleRefactoring :: Refactoring
                      }
  | ProjectRefactoring { refactoringName :: String
                       , projectRefactoring :: ProjectRefactoring
                       }

-- | Executes a given command (choosen from the set of available refactorings) on the selected
-- module and given other modules.
performCommand :: [RefactoringChoice] -- ^ The set of available refactorings
                    -> [String] -- ^ The refactoring command
                    -> Either FilePath ModuleDom -- ^ The module in which the refactoring is performed
                    -> [ModuleDom] -- ^ Other modules
                    -> Ghc (Either String [RefactorChange])
performCommand refactorings (name:args) mod mods =
    case (refactoring, mod, args) of
      (Just (NamingRefactoring _ trf), Right mod, (sp:newName:_))
        -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) newName mod mods
      (Just (NamingRefactoring _ _), Right _, _)
        -> return $ Left $ "The refactoring '" ++ name
                             ++ "' needs two argument: a source range and a name"
      (Just (SelectionRefactoring _ trf), Right mod, (sp:_))
        -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) mod mods
      (Just (SelectionRefactoring _ _), Right _, _)
        -> return $ Left $ "The refactoring '" ++ name ++ "' needs one argument: a source range"
      (Just (ModuleRefactoring _ trf), Right mod, _) -> runExceptT $ trf mod mods
      (Just (ProjectRefactoring _ trf), _, _) -> runExceptT $ trf mods
      (Just _, Left modPath, _)
        -> return $ Left $ "The following file is not loaded to Haskell-tools: " ++ modPath
                             ++ ". Please add the containing package."
      (Nothing, _, _) -> return $ Left $ "Unknown command: " ++ name
  where refactoring = find ((== name) . refactoringName) refactorings

-- | Gets the name of possible refactorings.
refactorCommands :: [RefactoringChoice] -> [String]
refactorCommands = map refactoringName
