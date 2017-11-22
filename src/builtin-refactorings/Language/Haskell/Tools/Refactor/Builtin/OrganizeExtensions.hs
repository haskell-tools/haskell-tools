{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, TupleSections, TypeFamilies, LambdaCase #-}

module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  ( module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ) where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.SupportedExtensions (unregularExts, isSupported, fullyHandledExtensions)

import Language.Haskell.Tools.Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions (expandExtension)

import GHC (Ghc(..))

import Control.Reference
import Data.Char (isAlpha)
import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as SMap (keys, empty)

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

type OrganizeExtensionsDomain dom = (HasModuleInfo dom, ExtDomain dom)

organizeExtensionsRefactoring :: OrganizeExtensionsDomain dom => RefactoringChoice dom
organizeExtensionsRefactoring = ModuleRefactoring "OrganizeExtensions" (localRefactoring organizeExtensions)

projectOrganizeExtensionsRefactoring :: OrganizeExtensionsDomain dom => RefactoringChoice dom
projectOrganizeExtensionsRefactoring = ProjectRefactoring "ProjectOrganizeExtensions" projectOrganizeExtensions

projectOrganizeExtensions :: forall dom . OrganizeExtensionsDomain dom => ProjectRefactoring dom
projectOrganizeExtensions =
  mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (organizeExtensions m))

tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . const organizeExtensions)

organizeExtensions :: ExtDomain dom => LocalRefactoring dom
organizeExtensions moduleAST = do
  exts <- liftGhc $ reduceExtensions moduleAST
  let isRedundant e = extName `notElem` foundExts && extName `elem` handledExts
        where extName = unregularExts (e ^. langExt)
      handledExts = map show fullyHandledExtensions
      foundExts = map show exts

  -- remove unused extensions (only those that are fully handled)
  filePragmas & annList & lpPragmas !~ filterListSt (not . isRedundant)
        -- remove empty {-# LANGUAGE #-} pragmas
    >=> filePragmas !~ filterListSt (\case LanguagePragma (AnnList []) -> False; _ -> True)
    $ moduleAST

-- | Reduces default extension list (keeps unsupported extensions)
reduceExtensions :: ExtDomain dom => UnnamedModule dom -> Ghc [Extension]
reduceExtensions = \moduleAST -> do
  let expanded = expandDefaults moduleAST
      (xs, ys) = partition isSupported expanded
  xs' <- flip execStateT SMap.empty . flip runReaderT xs . traverseModule $ moduleAST
  return . sortBy (compare `on` show) . mergeInduced . nub $ (calcExts xs' ++ ys)

  where isLVar (LVar _) = True
        isLVar _        = False

        calcExts :: ExtMap -> [Extension]
        calcExts logRels
          | ks <- SMap.keys logRels
          , all isLVar ks
          = map (\(LVar x) -> x) . SMap.keys $ logRels
          | otherwise     = []

        rmInduced :: Extension -> [Extension] -> [Extension]
        rmInduced e = flip (\\) induced
          where induced = delete e $ expandExtension e

        mergeInduced :: [Extension] -> [Extension]
        mergeInduced exts = foldl (flip rmInduced) exts exts


-- | Collects extensions induced by the source code (with location info)
collectExtensions :: ExtDomain dom => UnnamedModule dom -> Ghc ExtMap
collectExtensions moduleAST = do
  let expanded = expandDefaults moduleAST
  flip execStateT SMap.empty . flip runReaderT expanded . traverseModule $ moduleAST

-- | Collects default extension list, and expands each extension
expandDefaults :: UnnamedModule dom -> [Extension]
expandDefaults = nub . concatMap expandExtension . collectDefaultExtensions

-- | Collects extensions enabled by default
collectDefaultExtensions :: UnnamedModule dom -> [Extension]
collectDefaultExtensions = map toExt . getExtensions
  where
  getExtensions :: UnnamedModule dom -> [String]
  getExtensions = flip (^?) (filePragmas & annList & lpPragmas & annList & langExt)

toExt :: String -> Extension
toExt str = case map fst . reads . unregularExts . takeWhile isAlpha $ str of
              e:_ -> e
              []  -> error $ "Extension '" ++ takeWhile isAlpha str ++ "' is not known."
