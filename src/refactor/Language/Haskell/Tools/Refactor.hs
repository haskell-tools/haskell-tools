{-# LANGUAGE StandaloneDeriving
           , DeriveGeneric
           , LambdaCase
           , ScopedTypeVariables
           , BangPatterns
           , MultiWayIf
           , FlexibleContexts
           , TypeFamilies
           , TupleSections
           #-}
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
import Language.Haskell.Tools.Refactor.RangeDebug
import Language.Haskell.Tools.Refactor.RangeDebug.Instances
import Language.Haskell.Tools.Refactor.ASTDebug
import Language.Haskell.Tools.Refactor.ASTDebug.Instances

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
import Data.Time.Clock
import Data.IORef
import Data.Either.Combinators
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.DebugGhcAST
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

type RefactorSession = StateT RefactorSessionState

refactorSession :: [String] -> [String] -> IO ()
refactorSession workingDirs args = runGhc (Just libdir) $ flip evalStateT initSession $
  do lift $ useDirsAndFlags workingDirs args
     moduleNames <- liftIO $ concat <$> mapM (\wd -> map (wd,) <$> getModules wd) workingDirs
     lift $ setTargets (map (\(_,mod) -> (Target (TargetModule (mkModuleName mod)) True Nothing)) moduleNames)
     lift $ load LoadAllTargets
     allMods <- lift getModuleGraph
     mods <- lift $ forM allMods (\ms -> do mm <- parseTyped ms
                                            liftIO $ putStrLn ("Loaded module: " ++ (GHC.moduleNameString $ moduleName $ ms_mod ms))
                                            let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
                                                Just wd = find ((modName ==) . snd) moduleNames 
                                            return ((fst wd, modName, case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot), mm))
     modify $ \s -> s { refSessMods = Map.fromList mods }
     runSession
  where runSession :: RefactorSession Ghc ()
        runSession = do cmd <- liftIO $ getLine 
                        sessionComm <- readSessionCommand cmd
                        performSessionCommand sessionComm
                        doExit <- gets exiting 
                        when (not doExit) runSession

data RefactorSessionCommand 
  = LoadModule String
  | Exit
  | RefactorCommand RefactorCommand

readSessionCommand :: Monad m => String -> RefactorSession m RefactorSessionCommand
readSessionCommand cmd = case splitOn " " cmd of 
    ["LoadModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    _ -> do actualMod <- gets actualMod 
            case actualMod of Just (wd,m,_) -> return $ RefactorCommand $ readCommand (toFileName wd m) cmd
                              Nothing -> error "Set the actual module first"

performSessionCommand :: RefactorSessionCommand -> RefactorSession Ghc ()
performSessionCommand (LoadModule mod) = do fnd <- gets (find (\(_,m,hs) -> m == mod && hs == NormalHs) . Map.keys . refSessMods)
                                            if isJust fnd then modify $ \s -> s { actualMod = fnd }
                                                          else liftIO $ putStrLn ("Cannot find module: " ++ mod)
performSessionCommand Exit = modify $ \s -> s { exiting = True }
performSessionCommand (RefactorCommand cmd) 
  = do RefactorSessionState { refSessMods = mods, actualMod = Just act@(_, mod, _) } <- get
       res <- lift $ performCommand cmd (mod, mods Map.! act) (map (\((_,m,_),mod) -> (m,mod)) $ Map.assocs (Map.delete act mods))
       case res of Left err -> liftIO $ putStrLn err
                   Right resMods -> do 
                     mss <- forM resMods $ \case 
                       ContentChanged (n,m) -> do
                         let modName = semanticsModule $ m ^. semantics
                         ms <- getModSummary modName (isBootModule $ m ^. semantics)
                         let isBoot = case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot
                         Just (workingDir,_,_) <- gets (find (\(_,m,b) -> m == n && b == isBoot) . Map.keys . refSessMods)
                         liftIO $ withBinaryFile ((case isBoot of NormalHs -> toFileName; IsHsBoot -> toBootFileName) workingDir n) 
                                                 WriteMode (`hPutStr` prettyPrint m)
                         return $ Just (n, workingDir, modName, isBoot)
                       ModuleRemoved mod -> do
                         Just (workingDir,_,_) <- gets (find (\(_,m,b) -> m == mod) . Map.keys . refSessMods)
                         liftIO $ removeFile (toFileName workingDir mod)
                         modify $ \s -> s { refSessMods = Map.delete (workingDir, mod, IsHsBoot) $ Map.delete (workingDir, mod, NormalHs) (refSessMods s) }
                         return Nothing
                     lift $ load LoadAllTargets
                     forM_ (catMaybes mss) $ \(n, workingDir, modName, isBoot) -> do
                         -- TODO: add target if module is added as a change
                         ms <- getModSummary modName (isBoot == IsHsBoot)
                         newm <- lift $ parseTyped ms
                         modify $ \s -> s { refSessMods = Map.insert (workingDir, n, isBoot) newm (refSessMods s) }
                         liftIO $ putStrLn ("Re-loaded module: " ++ n)
  where getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods 

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty Nothing False

data IsBoot = NormalHs | IsHsBoot deriving (Eq, Ord, Show)

data RefactorSessionState
  = RefactorSessionState { refSessMods :: Map.Map (String, String, IsBoot) (UnnamedModule IdDom)
                         , actualMod :: Maybe (String, String, IsBoot)
                         , exiting :: Bool
                         }
    
data RefactorCommand = NoRefactor 
                     | OrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
    deriving Show

performCommand :: (SemanticInfo' dom SameInfoModuleCls ~ AST.ModuleInfo n, DomGenerateExports dom, OrganizeImportsDomain dom n, DomainRenameDefinition dom, ExtractBindingDomain dom, GenerateSignatureDomain dom) 
               => RefactorCommand -> ModuleDom dom -- ^ The module in which the refactoring is performed
                                  -> [ModuleDom dom] -- ^ Other modules
                                  -> Ghc (Either String [RefactorChange dom])
performCommand rf mod mods = runRefactor mod mods $ selectCommand rf
  where selectCommand NoRefactor = localRefactoring return
        selectCommand OrganizeImports = localRefactoring organizeImports
        selectCommand GenerateExports = localRefactoring generateExports 
        selectCommand (GenerateSignature sp) = localRefactoring $ generateTypeSignature' sp
        selectCommand (RenameDefinition sp str) = renameDefinition' sp str
        selectCommand (ExtractBinding sp str) = localRefactoring $ extractBinding' sp str

readCommand :: String -> String -> RefactorCommand
readCommand fileName s = case splitOn " " s of 
  [""] -> NoRefactor
  ("CheckSource":_) -> NoRefactor
  ("OrganizeImports":_) -> OrganizeImports
  ("GenerateExports":_) -> GenerateExports
  ["GenerateSignature", sp] -> GenerateSignature (readSrcSpan fileName sp)
  ["RenameDefinition", sp, name] -> RenameDefinition (readSrcSpan fileName sp) name
  ["ExtractBinding", sp, name] -> ExtractBinding (readSrcSpan fileName sp) name
  
readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)

performRefactor :: String -> FilePath -> [String] -> String -> IO (Either String String)
performRefactor command workingDir flags target = 
  runGhc (Just libdir) $ do
    useDirsAndFlags [workingDir] flags
    (mapRight newContent <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact m = performCommand (readCommand (toFileName workingDir target) command) (target,m) []
        newContent (ContentChanged (_, newContent) : ress) = prettyPrint newContent
        newContent (_ : ress) = newContent ress

useDirsAndFlags :: [FilePath] -> [String] -> Ghc ()
useDirsAndFlags workingDirs args = do 
  initGhcFlags workingDirs
  let lArgs = map (L noSrcSpan) args
  dynflags <- getSessionDynFlags
  let ((leftovers, errors, warnings), newDynFlags) = (runCmdLine $ processArgs flagsAll lArgs) dynflags
  setSessionDynFlags newDynFlags { importPaths = importPaths newDynFlags ++ workingDirs }
  return ()

initGhcFlags :: [FilePath] -> Ghc ()
initGhcFlags wds = do
  dflags <- getSessionDynFlags
  setSessionDynFlags 
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ dflags { importPaths = wds
             , hscTarget = HscAsm -- needed for static pointers
             , ghcLink = LinkInMemory
             , ghcMode = CompManager 
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }
  return ()

toFileName :: String -> String -> FilePath
toFileName workingDir mod = workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs"

toBootFileName :: String -> String -> FilePath
toBootFileName workingDir mod = workingDir </> map (\case '.' -> pathSeparator; c -> c) mod ++ ".hs-boot"

performRefactors :: String -> String -> [String] -> String -> IO (Either String [(String, Maybe String)])
performRefactors command workingDir flags target = do 
  mods <- getModules workingDir
  runGhc (Just libdir) $ do
    useDirsAndFlags [workingDir] flags
    setTargets (map (\mod -> (Target (TargetModule (mkModuleName mod)) True Nothing)) mods)
    load LoadAllTargets
    allMods <- getModuleGraph
    selectedMod <- getModSummary (mkModuleName target)
    let otherModules = filter (not . (\ms -> ms_mod ms == ms_mod selectedMod && ms_hsc_src ms == ms_hsc_src selectedMod)) allMods 
    targetMod <- parseTyped selectedMod
    otherMods <- mapM parseTyped otherModules
    res <- performCommand (readCommand (toFileName workingDir target) command) 
                          (target, targetMod) (zip (map (GHC.moduleNameString . moduleName . ms_mod) otherModules) otherMods)
    return $ mapRight (map (\case ContentChanged (n,m) -> (n, Just $ prettyPrint m)
                                  ModuleRemoved m -> (m, Nothing)
                           )) res

astView :: String -> String -> IO String
astView workingDir target = 
  runGhc (Just libdir) $
    (astDebug <$> (parseTyped =<< loadModule workingDir target))

loadModule :: String -> String -> Ghc ModSummary
loadModule workingDir moduleName 
  = do initGhcFlags [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       load LoadAllTargets
       getModSummary $ mkModuleName moduleName
    
type TypedModule = Ann AST.Module IdDom SrcTemplateStage

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

type RenamedModule = Ann AST.Module (Dom GHC.Name) SrcTemplateStage

parseRenamed :: ModSummary -> Ghc RenamedModule
parseRenamed modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (getNormalComments $ snd annots) 
    <$> (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule modSum (pm_parsed_source p)
            runTrf (fst annots) (getPragmaComments $ snd annots)
              $ trfModuleRename modSum parseTrf
                  (fromJust $ tm_renamed_source tc) 
                  (pm_parsed_source p))

type ParsedModule = Ann AST.Module (Dom RdrName) SrcTemplateStage

parseAST :: ModSummary -> Ghc Language.Haskell.Tools.Refactor.ParsedModule
parseAST modSum = do
  p <- parseModule modSum
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule modSum $ pm_parsed_source p)          
    
-- | Should be only used for testing
demoRefactor :: String -> String -> [String] -> String -> IO ()
demoRefactor command workingDir args moduleName = 
  runGhc (Just libdir) $ do
    useDirsAndFlags [workingDir] args
    modSum <- loadModule workingDir moduleName
    p <- parseModule modSum
    t <- typecheckModule p
        
    let r = tm_renamed_source t
    let annots = pm_annotations $ tm_parsed_module t

    liftIO $ putStrLn $ show annots
    liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (pm_parsed_source p)
    liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (typecheckedSource t)
    liftIO $ putStrLn "=========== parsed:"
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule modSum (pm_parsed_source p)
    liftIO $ putStrLn $ srcInfoDebug parseTrf
    liftIO $ putStrLn "=========== typed:"
    transformed <- addTypeInfos (typecheckedSource t) =<< (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModuleRename modSum parseTrf (fromJust $ tm_renamed_source t) (pm_parsed_source p))
    liftIO $ putStrLn $ srcInfoDebug transformed
    liftIO $ putStrLn "=========== ranges fixed:"
    let commented = fixRanges $ placeComments (getNormalComments $ snd annots) transformed
    liftIO $ putStrLn $ srcInfoDebug commented
    liftIO $ putStrLn "=========== cut up:"
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ srcInfoDebug cutUp
    liftIO $ putStrLn $ show $ getLocIndices cutUp
    liftIO $ putStrLn $ show $ mapLocIndices (fromJust $ ms_hspp_buf $ pm_mod_summary p) (getLocIndices cutUp)
    liftIO $ putStrLn "=========== sourced:"
    let sourced = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) cutUp
    liftIO $ putStrLn $ srcInfoDebug sourced
    liftIO $ putStrLn "=========== pretty printed:"
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    transformed <- performCommand (readCommand (fromJust $ ml_hs_file $ ms_location modSum) command) (moduleName, sourced) []
    case transformed of 
      Right [ContentChanged (_, correctlyTransformed)] -> do
        liftIO $ putStrLn "=========== transformed AST:"
        liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
        liftIO $ putStrLn "=========== transformed & prettyprinted:"
        let prettyPrinted = prettyPrint correctlyTransformed
        liftIO $ putStrLn prettyPrinted
        liftIO $ putStrLn "==========="
      Left transformProblem -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn transformProblem
        liftIO $ putStrLn "==========="
  
deriving instance Generic SrcSpan
deriving instance (Generic sema, Generic src) => Generic (NodeInfo sema src)
