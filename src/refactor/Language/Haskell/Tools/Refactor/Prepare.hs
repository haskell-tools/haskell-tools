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
-- | Defines utility methods that prepare Haskell modules for refactoring
module Language.Haskell.Tools.Refactor.Prepare where

import CmdLineParser
import DynFlags
import FastString
import GHC hiding (loadModule)
import qualified GHC (loadModule)
import GHC.Paths ( libdir )
import Packages
import SrcLoc

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IntSet (member)
import Data.List ((\\), intersperse, isSuffixOf)
import Data.List.Split
import Data.Maybe
import Language.Haskell.TH.LanguageExtensions
import System.Directory
import System.FilePath

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Transform

-- | A quick function to try the refactorings
tryRefactor :: (RealSrcSpan -> Refactoring IdDom) -> String -> String -> IO ()
tryRefactor refact moduleName span
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs ["."]
      mod <- loadModule "." moduleName >>= parseTyped
      res <- runRefactor (SourceFileKey NormalHs moduleName, mod) [] 
               $ refact $ correctRefactorSpan mod $ readSrcSpan span 
      case res of Right r -> liftIO $ mapM_ (putStrLn . prettyPrint . snd . fromContentChanged) r
                  Left err -> liftIO $ putStrLn err

-- | Adjust the source range to be applied to the refactored module
correctRefactorSpan :: UnnamedModule dom -> RealSrcSpan -> RealSrcSpan
correctRefactorSpan mod sp = mkRealSrcSpan (updateSrcFile fileName $ realSrcSpanStart sp) 
                                           (updateSrcFile fileName $ realSrcSpanEnd sp)
  where fileName = case srcSpanStart $ getRange mod of RealSrcLoc loc -> srcLocFile loc 
                                                       _ -> error "correctRefactorSpan: no real span"
        updateSrcFile fn loc = mkRealSrcLoc fn (srcLocLine loc) (srcLocCol loc) 

-- | Set the given flags for the GHC session
useFlags :: [String] -> Ghc [String]
useFlags args = do 
  let lArgs = map (L noSrcSpan) args
  dynflags <- getSessionDynFlags
  -- TODO: print errors and warnings?
  let ((leftovers, _, _), newDynFlags) = (runCmdLine $ processArgs flagsAll lArgs) dynflags
  void $ setSessionDynFlags newDynFlags
  when (any ("-package-db" `isSuffixOf`) args) reloadPkgDb
  return $ map unLoc leftovers

-- | Reloads the package database based on the session flags
reloadPkgDb :: Ghc ()
reloadPkgDb = void $ setSessionDynFlags . fst =<< liftIO . initPackages . (\df -> df { pkgDatabase = Nothing }) 
                                              =<< getSessionDynFlags 

-- | Initialize GHC flags to default values that support refactoring
initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = initGhcFlags' True

-- | Sets up basic flags and settings for GHC
initGhcFlags' :: Bool -> Ghc ()
initGhcFlags' needsCodeGen = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags 
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ dflags { importPaths = []
             , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager 
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }

-- | Use the given source directories when searching for imported modules
useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

-- | Don't use the given source directories when searching for imported modules
deregisterDirs :: [FilePath] -> Ghc ()
deregisterDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags \\ workingDirs }
  
-- | Translates module name and working directory into the name of the file where the given module should be defined
toFileName :: FilePath -> String -> FilePath
toFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs"

-- | Translates module name and working directory into the name of the file where the boot module should be defined
toBootFileName :: FilePath -> String -> FilePath
toBootFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs-boot"

-- | Get the source directory where the module is located.
getSourceDir :: ModSummary -> IO FilePath
getSourceDir ms 
  = do filePath <- canonicalizePath $ getModSumOrig ms
       let modNameParts = splitOn "." $ GHC.moduleNameString (moduleName (ms_mod ms)) 
           filePathParts = splitPath filePath
       let srcDirParts = reverse $ drop (length modNameParts) $ reverse filePathParts
       return $ joinPath srcDirParts

-- | Gets the path to the source file of the module.
getModSumOrig :: ModSummary -> FilePath
getModSumOrig = normalise . fromMaybe (error "getModSumOrig: The given module doesn't have haskell source file.") . ml_hs_file . ms_location

-- | Load the summary of a module given by the working directory and module name.
loadModule :: String -> String -> Ghc ModSummary
loadModule workingDir moduleName 
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       void $ load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName
    
-- | The final version of our AST, with type infromation added
type TypedModule = Ann AST.UModule IdDom SrcTemplateStage

-- | Get the typed representation from a type-correct program.
parseTyped :: ModSummary -> Ghc TypedModule
parseTyped modSum = withAlteredDynFlags (return . normalizeFlags) $ do
  let hasStaticFlags = StaticPointers `xopt` ms_hspp_opts modSum
      hasCppExtension = Cpp `xopt` ms_hspp_opts modSum
      hasUnicodeExtension = UnicodeSyntax `xopt` ms_hspp_opts modSum
      ms = if hasStaticFlags then forceAsmGen (modSumNormalizeFlags modSum) else (modSumNormalizeFlags modSum)
  when (hasCppExtension || hasUnicodeExtension) 
    $ throw (IllegalExtensions (["CPP" | hasCppExtension] ++ ["UnicodeSyntax" | hasUnicodeExtension]))
  p <- parseModule ms
  tc <- typecheckModule p
  void $ GHC.loadModule tc -- when used with loadModule, the module will be loaded twice
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  prepareAST srcBuffer . placeComments (getNormalComments $ snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
                   runTrf (fst annots) (getPragmaComments $ snd annots)
                     $ trfModuleRename ms parseTrf
                         (fromJust $ tm_renamed_source tc) 
                         (pm_parsed_source p)))

-- | Modifies the dynamic flags for performing a ghc task
withAlteredDynFlags :: GhcMonad m => (DynFlags -> m DynFlags) -> m a -> m a
withAlteredDynFlags modDFs action = do
  dfs <- getSessionDynFlags
  void $ setSessionDynFlags =<< modDFs dfs
  res <- action
  void $ setSessionDynFlags dfs
  return res

-- | Forces the code generation for a given module
forceCodeGen :: ModSummary -> ModSummary
forceCodeGen ms = ms { ms_hspp_opts = modOpts' }
  where modOpts = (ms_hspp_opts ms) { hscTarget = HscInterpreted }
        modOpts' = modOpts { ghcLink = LinkInMemory }

-- | Forces ASM code generation for a given module
forceAsmGen :: ModSummary -> ModSummary
forceAsmGen ms = ms { ms_hspp_opts = modOpts' }
  where modOpts = (ms_hspp_opts ms) { hscTarget = HscAsm }
        modOpts' = modOpts { ghcLink = LinkInMemory }

-- | Normalizes the flags for a module summary
modSumNormalizeFlags :: ModSummary -> ModSummary
modSumNormalizeFlags ms = ms { ms_hspp_opts = normalizeFlags (ms_hspp_opts ms) }

-- | Removes all flags that are unintelligable for refactoring
normalizeFlags :: DynFlags -> DynFlags
normalizeFlags = updOptLevel 0

-- | Read a source range from our textual format: @line:col-line:col@ or @line:col@
readSrcSpan :: String -> RealSrcSpan
readSrcSpan s = case splitOn "-" s of
  [one] -> mkRealSrcSpan (readSrcLoc one) (readSrcLoc one)
  [from,to] -> mkRealSrcSpan (readSrcLoc from) (readSrcLoc to)
  
-- | Read a source location from our format: @line:col@
readSrcLoc :: String -> RealSrcLoc
readSrcLoc s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString "file-name-should-be-fixed") (read line) (read col)
  _ -> error "readSrcLoc: panic: splitOn gives empty list"