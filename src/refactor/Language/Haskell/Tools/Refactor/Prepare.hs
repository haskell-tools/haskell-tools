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

import Control.Monad.IO.Class
import System.FilePath
import Data.Maybe
import Data.List.Split

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
initGhcFlags = do
  dflags <- getSessionDynFlags
  setSessionDynFlags 
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ dflags { importPaths = []
             , hscTarget = HscAsm -- needed for static pointers
             , ghcLink = LinkInMemory
             , ghcMode = CompManager 
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }
  return () 

-- | Use the given source directories
useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }
  return ()
  
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
       load LoadAllTargets
       getModSummary $ mkModuleName moduleName
    
-- | The final version of our AST, with type infromation added
type TypedModule = Ann AST.UModule IdDom SrcTemplateStage

-- | Get the typed representation from a type-correct program.
parseTyped :: ModSummary -> Ghc TypedModule
parseTyped modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  prepareAST srcBuffer . placeComments (getNormalComments $ snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule modSum (pm_parsed_source p)
                   runTrf (fst annots) (getPragmaComments $ snd annots)
                     $ trfModuleRename modSum parseTrf
                         (fromJust $ tm_renamed_source tc) 
                         (pm_parsed_source p)))

data IsBoot = NormalHs | IsHsBoot deriving (Eq, Ord, Show)

readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)