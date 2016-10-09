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
module Language.Haskell.Tools.Refactor where

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.PlaceComments
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint

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
 
import Data.List
import Data.List.Split
import GHC.Generics hiding (moduleName)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.OrganizeImports
import Language.Haskell.Tools.Refactor.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.GenerateExports
import Language.Haskell.Tools.Refactor.RenameDefinition
import Language.Haskell.Tools.Refactor.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.GetModules

import Language.Haskell.TH.LanguageExtensions
 
import DynFlags
import StringBuffer            

import Debug.Trace


-- | Use the given source directories
useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }
  return ()

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
type TypedModule = Ann AST.Module IdDom SrcTemplateStage

-- | Get the typed representation from a type-correct program.
parseTyped :: ModSummary -> Ghc TypedModule
parseTyped modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (getNormalComments $ snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule modSum (pm_parsed_source p)
                   runTrf (fst annots) (getPragmaComments $ snd annots)
                     $ trfModuleRename modSum parseTrf
                         (fromJust $ tm_renamed_source tc) 
                         (pm_parsed_source p)))

-- | Executes a given command on the selected module and given other modules
performCommand :: (HasModuleInfo dom, DomGenerateExports dom, OrganizeImportsDomain dom, DomainRenameDefinition dom, ExtractBindingDomain dom, GenerateSignatureDomain dom) 
               => RefactorCommand -> ModuleDom dom -- ^ The module in which the refactoring is performed
                                  -> [ModuleDom dom] -- ^ Other modules
                                  -> Ghc (Either String [RefactorChange dom])
performCommand rf mod mods = runRefactor mod mods $ selectCommand rf
  where selectCommand NoRefactor = localRefactoring return
        selectCommand OrganizeImports = localRefactoring organizeImports
        selectCommand GenerateExports = localRefactoring generateExports 
        selectCommand (GenerateSignature sp) = localRefactoring $ generateTypeSignature' (correctSp mod sp)
        selectCommand (RenameDefinition sp str) = renameDefinition' (correctSp mod sp) str
        selectCommand (ExtractBinding sp str) = localRefactoring $ extractBinding' (correctSp mod sp) str

        correctSp mod sp = mkRealSrcSpan (updateSrcFile fileName $ realSrcSpanStart sp) 
                                         (updateSrcFile fileName $ realSrcSpanEnd sp)
        fileName = case srcSpanStart $ getRange (snd mod) of RealSrcLoc loc -> srcLocFile loc 
        updateSrcFile fn loc = mkRealSrcLoc fn (srcLocLine loc) (srcLocCol loc) 

-- | A refactoring command
data RefactorCommand = NoRefactor 
                     | OrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
    deriving Show

readCommand :: String -> String -> RefactorCommand
readCommand fileName (splitOn " " -> refact:args) = analyzeCommand fileName refact args

analyzeCommand :: String -> String -> [String] -> RefactorCommand
analyzeCommand _ "" _ = NoRefactor
analyzeCommand _ "CheckSource" _ = NoRefactor
analyzeCommand _ "OrganizeImports" _ = OrganizeImports
analyzeCommand _ "GenerateExports" _ = GenerateExports
analyzeCommand fileName "GenerateSignature" [sp] = GenerateSignature (readSrcSpan fileName sp)
analyzeCommand fileName "RenameDefinition" [sp, newName] = RenameDefinition (readSrcSpan fileName sp) newName
analyzeCommand fileName "ExtractBinding" [sp, newName] = ExtractBinding (readSrcSpan fileName sp) newName
analyzeCommand _ ref _ = error $ "Unknown command: " ++ ref

readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)

data IsBoot = NormalHs | IsHsBoot deriving (Eq, Ord, Show)

tryRefactor :: Refactoring IdDom -> String -> IO ()
tryRefactor refact moduleName 
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs ["."]
      mod <- loadModule "." moduleName >>= parseTyped
      res <- runRefactor (toFileName "." moduleName, mod) [] refact 
      case res of Right r -> liftIO $ mapM_ (putStrLn . prettyPrint . snd . fromContentChanged) r
                  Left err -> liftIO $ putStrLn err
