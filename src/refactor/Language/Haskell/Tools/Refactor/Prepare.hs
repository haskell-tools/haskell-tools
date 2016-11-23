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

import GHC hiding (loadModule)
import qualified GHC (loadModule)
import Panic (handleGhcException)
import Outputable
import BasicTypes
import Bag
import Var
import SrcLoc
import Module as GHC
import FastString
import HscTypes
import GHC.Paths ( libdir )
import CmdLineParser
import DynFlags
import StringBuffer

import Control.Monad
import Control.Monad.IO.Class
import System.FilePath
import Data.Maybe
import Data.List (isInfixOf)
import Data.List.Split
import System.Info (os)
import Data.IntSet (member)
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Transform
import Language.Haskell.Tools.Refactor.RefactorBase

tryRefactor :: Refactoring IdDom -> String -> IO ()
tryRefactor refact moduleName 
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs ["."]
      mod <- loadModule "." moduleName >>= parseTyped
      res <- runRefactor (toFileName "." moduleName, mod) [] refact 
      case res of Right r -> liftIO $ mapM_ (putStrLn . prettyPrint . snd . fromContentChanged) r
                  Left err -> liftIO $ putStrLn err

-- | Set the given flags for the GHC session
useFlags :: [String] -> Ghc [String]
useFlags args = do 
  let lArgs = map (L noSrcSpan) args
  dynflags <- getSessionDynFlags
  let ((leftovers, errors, warnings), newDynFlags) = (runCmdLine $ processArgs flagsAll lArgs) dynflags
  setSessionDynFlags newDynFlags
  return $ map unLoc leftovers

-- | Initialize GHC flags to default values that support refactoring
initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = initGhcFlags' True

initGhcFlags' :: Bool -> Ghc ()
initGhcFlags' needsCodeGen = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags 
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ dflags { importPaths = []
             , hscTarget = if needsCodeGen || ("linux" `isInfixOf` os) then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen || ("linux" `isInfixOf` os) then LinkInMemory else NoLink
             , ghcMode = CompManager 
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }

-- | Use the given source directories
useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }
  
-- | Translates module name and working directory into the name of the file where the given module should be defined
toFileName :: String -> String -> FilePath
toFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs"

-- | Translates module name and working directory into the name of the file where the boot module should be defined
toBootFileName :: String -> String -> FilePath
toBootFileName workingDir mod = normalise $ workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs-boot"

-- | Load the summary of a module given by the working directory and module name.
loadModule :: String -> String -> Ghc ModSummary
loadModule workingDir moduleName 
  = do initGhcFlags
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName
    
-- | The final version of our AST, with type infromation added
type TypedModule = Ann AST.UModule IdDom SrcTemplateStage

-- | Get the typed representation from a type-correct program.
parseTyped :: ModSummary -> Ghc TypedModule
parseTyped modSum = do
  let compExts = extensionFlags $ ms_hspp_opts modSum
      hasStaticFlags = fromEnum StaticPointers `member` compExts
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  tc <- typecheckModule p
  GHC.loadModule tc -- when used with loadModule, the module will be loaded twice
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  prepareAST srcBuffer . placeComments (getNormalComments $ snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
                   runTrf (fst annots) (getPragmaComments $ snd annots)
                     $ trfModuleRename ms parseTrf
                         (fromJust $ tm_renamed_source tc) 
                         (pm_parsed_source p)))

forceCodeGen :: ModSummary -> ModSummary
forceCodeGen ms = ms { ms_hspp_opts = modOpts' }
  where modOpts = (ms_hspp_opts ms) { hscTarget = HscInterpreted }
        modOpts' = modOpts { ghcLink = LinkInMemory }

forceAsmGen :: ModSummary -> ModSummary
forceAsmGen ms = ms { ms_hspp_opts = modOpts' }
  where modOpts = (ms_hspp_opts ms) { hscTarget = HscAsm }
        modOpts' = modOpts { ghcLink = LinkInMemory }

readSrcSpan :: String -> RealSrcSpan
readSrcSpan s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc from) (readSrcLoc to)
  
readSrcLoc :: String -> RealSrcLoc
readSrcLoc s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString "file-name-should-be-fixed") (read line) (read col)