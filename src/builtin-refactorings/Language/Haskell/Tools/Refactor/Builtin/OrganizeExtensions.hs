{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}


module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  ( module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ) where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.SupportedExtensions (isSupported)

import Language.Haskell.Tools.Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions

import GHC (Ghc(..))

import Control.Reference
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty)

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

organizeExtensionsRefactoring :: RefactoringChoice
organizeExtensionsRefactoring = ModuleRefactoring "OrganizeExtensions" (localRefactoring organizeExtensions)

projectOrganizeExtensionsRefactoring :: RefactoringChoice
projectOrganizeExtensionsRefactoring = ProjectRefactoring "ProjectOrganizeExtensions" projectOrganizeExtensions

projectOrganizeExtensions :: ProjectRefactoring
projectOrganizeExtensions =
  mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (organizeExtensions m))

tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . const organizeExtensions)

organizeExtensions :: LocalRefactoring
organizeExtensions moduleAST = do
  exts <- liftGhc $ reduceExtensions moduleAST
  let langExts = map (mkLanguagePragma . pure . serializeExt . show) exts
      ghcOpts  = moduleAST ^? filePragmas & annList & opStr & stringNodeStr
      ghcOpts' = map (mkOptionsGHC . unwords . filter (isPrefixOf "-") . words) ghcOpts

      offExts  = map (mkLanguagePragma . pure)
               . sort
               . map (("No" ++) . serializeExt . show)
               . collectTurnedOffExtensions
               $ moduleAST

      newPragmas = mkFilePragmas $ offExts ++ langExts ++ ghcOpts'

  (filePragmas != newPragmas)
    -- remove empty {-# LANGUAGE #-} pragmas
    >=> filePragmas !~ filterListSt (\case LanguagePragma (AnnList []) -> False; _ -> True)
    $ moduleAST

-- | Reduces default extension list (keeps unsupported extensions)
reduceExtensions :: UnnamedModule -> Ghc [Extension]
reduceExtensions moduleAST = do
  let defaults = map replaceDeprecated . collectDefaultExtensions $ moduleAST
      expanded = expandExtensions defaults
      (xs, ys) = partition isSupported expanded

  xs' <- flip execStateT SMap.empty . flip runReaderT xs . traverseModule $ moduleAST
  let filteredExts = nub . mergeImplied $ (determineExtensions xs' ++ ys)
  if any (`elem` filteredExts) [Cpp, TemplateHaskell, TemplateHaskellQuotes, QuasiQuotes]
    -- We can't say anything about generated code
    then return . mergeImplied $ defaults
    -- Merging is needed because there might be unsopported extensions
    -- that are implied by supported extensions (TypeFamilies -> MonoLocalBinds)
    else return . sortBy (compare `on` show) $ filteredExts

--

-- | Collects extensions induced by the source code (with location info)
collectExtensions :: UnnamedModule -> Ghc ExtMap
collectExtensions = collectExtensionsWith traverseModule

collectExtensionsWith :: CheckNode UnnamedModule -> UnnamedModule -> Ghc ExtMap
collectExtensionsWith trvModule moduleAST = do
  let expanded = expandExtensions . collectDefaultExtensions $ moduleAST
  flip execStateT SMap.empty . flip runReaderT expanded . trvModule $ moduleAST


-- | Expands every extension in a list, while not producing any duplicates.
expandExtensions :: [Extension] -> [Extension]
expandExtensions = nub . concatMap expandExtension

-- | Collects extensions enabled by default
collectDefaultExtensions :: UnnamedModule -> [Extension]
collectDefaultExtensions = mapMaybe toExt . getExtensions

-- | Collects extensions enabled by default
collectTurnedOffExtensions :: UnnamedModule -> [Extension]
collectTurnedOffExtensions = mapMaybe (toExt . drop 2)
                           . filter (isPrefixOf "No")
                           . getExtensions

-- | Collects the string representation of the extensions in the module
getExtensions :: UnnamedModule -> [String]
getExtensions = flip (^?) (filePragmas & annList & lpPragmas & annList & langExt)

toExt :: String -> Maybe Extension
toExt str = case map fst . reads . canonExt . takeWhile isAlphaNum $ str of
              e:_ -> Just e
              []  -> fail $ "Extension '" ++ takeWhile isAlphaNum str ++ "' is not known."
