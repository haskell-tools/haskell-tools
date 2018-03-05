module Language.Haskell.Tools.Refactor.Querying where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.List ((++), map, find)
import Data.Aeson (Value)

import GHC (RealSrcSpan, Ghc)

import Language.Haskell.Tools.AST ()
import Language.Haskell.Tools.Refactor.Monad (ProjectRefactoring, Refactoring)
import Language.Haskell.Tools.Refactor.Prepare (correctRefactorSpan, readSrcSpan)
import Language.Haskell.Tools.Refactor.Representation (RefactorChange, ModuleDom)

data QueryChoice = LocationQuery
                     { queryName :: String
                     , locationQuery :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
                     }

type QueryMonad = ExceptT String Ghc

queryCommands :: [QueryChoice] -> [String]
queryCommands = map queryName

queryError :: String -> QueryMonad a
queryError = throwE

performQuery :: [QueryChoice] -- ^ The set of available queries
                  -> [String] -- ^ The query command
                  -> Either FilePath ModuleDom -- ^ The module in which the refactoring is performed
                  -> [ModuleDom] -- ^ Other modules
                  -> Ghc (Either String Value)
performQuery queries (name:args) mod mods =
  case (query, mod, args) of
    (Just (LocationQuery _ query), Right mod, sp:_)
      -> runExceptT $ query (correctRefactorSpan (snd mod) $ readSrcSpan sp) mod mods
    (Just (LocationQuery _ query), _, _)
      -> return $ Left $ "The query '" ++ name ++ "' needs one argument: a source range"
    (Nothing, _, _)
      -> return $ Left $ "Unknown command: " ++ name
  where query = find ((== name) . queryName) queries
