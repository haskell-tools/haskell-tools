{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TemplateHaskell
           , TypeFamilies
           , StandaloneDeriving
           #-}
module Language.Haskell.Tools.Refactor.CLI (refactorSession, tryOut) where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Control.Monad.State.Strict
import Control.Reference
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import System.Directory
import System.Exit
import System.IO
import System.FilePath

import DynFlags as GHC
import ErrUtils
import GHC
import GHC.Paths ( libdir )
import HscTypes as GHC
import Outputable
import Packages

import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor as HT
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Session

type CLIRefactorSession = StateT CLISessionState Ghc

data CLISessionState =
  CLISessionState { _refactState :: RefactorSessionState
                  , _actualMod :: Maybe SourceFileKey
                  , _exiting :: Bool
                  , _dryMode :: Bool
                  }

makeReferences ''CLISessionState

deriving instance Show PkgConfRef

tryOut :: IO ()
tryOut = void $ refactorSession stdin stdout
                  [ "-dry-run", "-one-shot", "-module-name=Language.Haskell.Tools.AST", "-refactoring=OrganizeImports"
                  , "src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

refactorSession :: Handle -> Handle -> [String] -> IO Bool
refactorSession input output args = runGhc (Just libdir) $ handleSourceError printSrcErrors
                                                         $ flip evalStateT initSession $
  do lift $ initGhcFlags
     workingDirsAndHtFlags <- lift $ useFlags args
     let (htFlags, workingDirs) = partition (\case ('-':_) -> True; _ -> False) workingDirsAndHtFlags
     if null workingDirs then do liftIO $ hPutStrLn output usageMessage
                                 return False
                         else do initSuccess <- initializeSession output workingDirs htFlags
                                 when initSuccess $ runSession input output htFlags
                                 return initSuccess

  where printSrcErrors err = do dfs <- getSessionDynFlags
                                liftIO $ printBagOfErrors dfs (srcErrorMessages err)
                                return False

        initializeSession :: Handle -> [FilePath] -> [String] -> CLIRefactorSession Bool
        initializeSession output workingDirs flags = do
          liftIO $ hSetBuffering output NoBuffering
          liftIO $ hPutStrLn output "Compiling modules. This may take some time. Please wait."
          res <- loadPackagesFrom (\ms -> liftIO $ hPutStrLn output ("Loaded module: " ++ modSumName ms)) (const $ return ()) (\_ _ -> return []) workingDirs
          case res of
            Right _ -> do
              liftIO . hPutStrLn output $ if ("-one-shot" `elem` flags)
                then "All modules loaded."
                else "All modules loaded. Use 'SelectModule module-name' to select a module."
              when ("-dry-run" `elem` flags) $ modify (dryMode .= True)
              return True
            Left err -> liftIO $ do hPutStrLn output (displayException err)
                                    return False

        runSession :: Handle -> Handle -> [String] -> CLIRefactorSession ()
        runSession _ output flags | "-one-shot" `elem` flags
          = let modName = catMaybes $ map (\f -> case splitOn "=" f of ["-module-name", mod] -> Just mod; _ -> Nothing) flags
                refactoring = catMaybes $ map (\f -> case splitOn "=" f of ["-refactoring", ref] -> Just ref; _ -> Nothing) flags
             in case (modName, refactoring) of
                  ([modName],[refactoring]) ->
                    do performSessionCommand output (LoadModule modName)
                       command <- readSessionCommand output (takeWhile (/='"') $ dropWhile (=='"') $ refactoring)
                       void $ performSessionCommand output command
                  ([],["ProjectOrganizeImports"]) ->
                    void $ performSessionCommand output (RefactorCommand ProjectOrganizeImports)
                  _ -> liftIO $ hPutStrLn output "-module-name or -refactoring flag not specified correctly. Not doing any refactoring."
        runSession input output _ = runSessionLoop input output

        runSessionLoop :: Handle -> Handle -> CLIRefactorSession ()
        runSessionLoop input output = do
          actualMod <- gets (^. actualMod)
          liftIO $ hPutStr output (maybe "no-module-selected> " (\sfk -> (sfk ^. sfkModuleName) ++ "> ") actualMod)
          cmd <- liftIO $ hGetLine input
          sessionComm <- readSessionCommand output cmd
          changedMods <- performSessionCommand output sessionComm
          void $ reloadChangedModules (hPutStrLn output . ("Re-loaded module: " ++) . modSumName) (const $ return ())
                   (\ms -> keyFromMS ms `elem` changedMods)
          doExit <- gets (^. exiting)
          when (not doExit) (void (runSessionLoop input output))

        usageMessage = "Usage: ht-refact [ht-flags, ghc-flags] package-pathes\n"
                         ++ "ht-flags: -dry-run -one-shot -module-name=modulename -refactoring=\"refactoring\""

data RefactorSessionCommand
  = LoadModule String
  | Skip
  | Exit
  | RefactorCommand RefactorCommand
  deriving Show

readSessionCommand :: Handle -> String -> CLIRefactorSession RefactorSessionCommand
readSessionCommand output cmd = case (splitOn " " cmd) of
    ["SelectModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    cm | head cm `elem` refactorCommands
       -> do actualMod <- gets (^. actualMod)
             case readCommand cmd of
               Right cmd ->
                 case actualMod of Just _ -> return $ RefactorCommand cmd
                                   Nothing -> do liftIO $ hPutStrLn output "Set the actual module first"
                                                 return Skip
               Left err -> do liftIO $ hPutStrLn output err
                              return Skip
    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: SelectModule, Exit, "
                                            ++ intercalate ", " refactorCommands
            return Skip

performSessionCommand :: Handle -> RefactorSessionCommand -> CLIRefactorSession [SourceFileKey]
performSessionCommand output (LoadModule modName) = do
  files <- HT.findModule modName
  case files of
    [] -> liftIO $ hPutStrLn output ("Cannot find module: " ++ modName)
    [fileName] -> do
      mod <- gets (lookupModInSCs (SourceFileKey fileName modName) . (^. refSessMCs))
      modify $ actualMod .= fmap fst mod
    _ -> liftIO $ hPutStrLn output ("Ambiguous module: " ++ modName ++ " found: " ++ show files)
  return []
performSessionCommand _ Skip = return []
performSessionCommand _ Exit = do modify $ exiting .= True
                                  return []
performSessionCommand output (RefactorCommand cmd)
  = do actMod <- gets (^. actualMod)
       (actualMod, otherMods) <- getMods actMod
       res <- case actualMod of
         Just mod -> lift $ performCommand cmd mod otherMods
         -- WALKAROUND: support running refactors that need no module selected
         Nothing -> case otherMods of (hd:rest) -> lift $ performCommand cmd hd rest
                                      []        -> return (Right [])
       inDryMode <- gets (^. dryMode)
       case res of Left err -> do liftIO $ hPutStrLn output err
                                  return []
                   Right resMods -> performChanges output inDryMode resMods

  where performChanges :: HasModuleInfo dom => Handle -> Bool -> [RefactorChange dom] -> CLIRefactorSession [SourceFileKey]
        performChanges output False resMods =
          forM resMods $ \case
            ModuleCreated n m otherM -> do
              Just (_, otherMR) <- gets (lookupModInSCs otherM . (^. refSessMCs))
              let Just otherMS = otherMR ^? modRecMS

              otherSrcDir <- liftIO $ getSourceDir otherMS
              let loc = srcDirFromRoot otherSrcDir n
              liftIO $ withBinaryFile loc WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle (prettyPrint m)
              return (SourceFileKey n (sourceFileModule (loc `makeRelative` n)))
            ContentChanged (n,m) -> do
              let modName = semanticsModule m
              ms <- getModSummary modName (isBootModule $ m ^. semantics)
              let file = fromJust $ ml_hs_file $ ms_location ms
              liftIO $ withBinaryFile file WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle (prettyPrint m)
              return n
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupSourceFileInSCs mod . (^. refSessMCs))
              case ( fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule)
                   , fmap isBootModule (m ^? typedRecModule) <|> fmap isBootModule (m ^? renamedRecModule)) of
                (Just modName, Just isBoot) -> do
                  ms <- getModSummary modName isBoot
                  let file = fromJust $ ml_hs_file $ ms_location ms
                  modify $ (refSessMCs .- removeModule mod)
                  liftIO $ removeFile file
                  return (SourceFileKey file mod)
                _ -> do liftIO $ hPutStrLn output ("Module " ++ mod ++ " could not be removed.")
                        return (SourceFileKey "" mod)

        performChanges output True resMods = do
          forM_ resMods (liftIO . \case
            ContentChanged (n,m) -> do
              hPutStrLn output $ "### Module changed: " ++ (n ^. sfkModuleName) ++ "\n### new content:\n" ++ prettyPrint m
            ModuleRemoved mod ->
              hPutStrLn output $ "### Module removed: " ++ mod
            ModuleCreated n m _ ->
              hPutStrLn output $ "### Module created: " ++ n ++ "\n### new content:\n" ++ prettyPrint m)
          return []

        getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods

instance IsRefactSessionState CLISessionState where
  refSessMCs = refactState & _refSessMCs
  initSession = CLISessionState initSession Nothing False False
