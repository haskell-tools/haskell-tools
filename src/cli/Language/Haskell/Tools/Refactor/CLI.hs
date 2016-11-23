{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TemplateHaskell
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Refactor.CLI (refactorSession) where

import System.Directory
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad.State
import Control.Applicative ((<|>))
import Control.Reference

import GHC
import Digraph
import HscTypes as GHC
import Module as GHC
import GHC.Paths ( libdir )

import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Session

import Debug.Trace

type CLIRefactorSession = StateT CLISessionState Ghc

data CLISessionState = 
  CLISessionState { _refactState :: RefactorSessionState
                  , _actualMod :: Maybe SourceFileKey
                  , _exiting :: Bool
                  , _dryMode :: Bool
                  }

makeReferences ''CLISessionState

tryOut = refactorSession stdin stdout 
           [ "-dry-run", "-one-shot", "-module-name=Language.Haskell.Tools.AST", "-refactoring=OrganizeImports"
           , "src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

refactorSession :: Handle -> Handle -> [String] -> IO ()
refactorSession input output args = runGhc (Just libdir) $ flip evalStateT initSession $
  do lift $ initGhcFlags
     workingDirsAndHtFlags <- lift $ useFlags args
     let (htFlags, workingDirs) = partition (\f -> head f == '-') workingDirsAndHtFlags
     if null workingDirs then liftIO $ hPutStrLn output usageMessage
                         else do initializeSession output workingDirs htFlags
                                 runSession input output htFlags
     
  where initializeSession :: Handle -> [FilePath] -> [String] -> CLIRefactorSession ()
        initializeSession output workingDirs flags = do
          liftIO $ hSetBuffering output NoBuffering
          liftIO $ hPutStrLn output "Compiling modules. This may take some time. Please wait."
          (_, ignoredMods) <- loadPackagesFrom (\m -> liftIO $ hPutStrLn output ("Loaded module: " ++ m)) workingDirs
          when (not $ null ignoredMods) 
            $ liftIO $ hPutStrLn output 
            $ "The following modules are ignored: " 
                ++ concat (intersperse ", " $ map (\(id,mod) -> mod ++ " (from " ++ moduleCollectionIdString id ++ ")") ignoredMods)
                ++ ". Multiple modules with the same qualified name are not supported."
          liftIO $ hPutStrLn output "All modules loaded. Use 'SelectModule module-name' to select a module"
          when ("-dry-run" `elem` flags) $ modify (dryMode .= True)

        runSession :: Handle -> Handle -> [String] -> CLIRefactorSession ()
        runSession _ output flags | "-one-shot" `elem` flags
          = let modName = catMaybes $ map (\f -> case splitOn "=" f of ["-module-name", mod] -> Just mod; _ -> Nothing) flags
                refactoring = catMaybes $ map (\f -> case splitOn "=" f of ["-refactoring", ref] -> Just ref; _ -> Nothing) flags
             in case (modName, refactoring) of 
                  ([modName],[refactoring]) ->
                    do performSessionCommand output (LoadModule modName)
                       command <- readSessionCommand output (takeWhile (/='"') $ dropWhile (=='"') $ refactoring)
                       performSessionCommand output command
                  _ -> liftIO $ hPutStrLn output usageMessage
        runSession input output _ = runSessionLoop input output

        runSessionLoop :: Handle -> Handle -> CLIRefactorSession ()
        runSessionLoop input output = do 
          actualMod <- gets (^. actualMod)
          liftIO $ hPutStr output (maybe "no-module-selected> " (\sfk -> (sfk ^. sfkModuleName) ++ "> ") actualMod)
          cmd <- liftIO $ hGetLine input 
          sessionComm <- readSessionCommand output cmd
          performSessionCommand output sessionComm
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
readSessionCommand output cmd = case splitOn " " cmd of 
    ["SelectModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    _ -> do actualMod <- gets (^. actualMod)
            case actualMod of Just _ -> return $ RefactorCommand $ readCommand cmd
                              Nothing -> do liftIO $ hPutStrLn output "Set the actual module first"
                                            return Skip

performSessionCommand :: Handle -> RefactorSessionCommand -> CLIRefactorSession ()
performSessionCommand output (LoadModule modName) = do 
  mod <- gets (lookupModInSCs (SourceFileKey NormalHs modName) . (^. refSessMCs))
  if isJust mod then modify $ actualMod .= fmap fst mod
                else liftIO $ hPutStrLn output ("Cannot find module: " ++ modName)
performSessionCommand _ Skip = return ()
performSessionCommand _ Exit = modify $ exiting .= True
performSessionCommand output (RefactorCommand cmd) 
  = do actMod <- gets (^. actualMod)
       (Just actualMod, otherMods) <- getMods actMod
       res <- lift $ performCommand cmd (assocToNamedMod actualMod) (map assocToNamedMod otherMods)
       inDryMode <- gets (^. dryMode)
       case res of Left err -> liftIO $ hPutStrLn output err
                   Right resMods -> performChanges output inDryMode resMods

  where performChanges output False resMods = do 
          changedMods <- forM resMods $ \case 
            ContentChanged (n,m) -> do
              let modName = semanticsModule m
              ms <- getModSummary modName (isBootModule $ m ^. semantics)
              let file = fromJust $ ml_hs_file $ ms_location ms
              liftIO $ withBinaryFile file WriteMode (`hPutStr` prettyPrint m)
              return n
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupModInSCs (SourceFileKey NormalHs mod) . (^. refSessMCs))
              case ( fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule)
                   , fmap isBootModule (m ^? typedRecModule) <|> fmap isBootModule (m ^? renamedRecModule)) of 
                (Just modName, Just isBoot) -> do
                  ms <- getModSummary modName isBoot
                  let file = fromJust $ ml_hs_file $ ms_location ms
                  modify $ (refSessMCs .- removeModule mod)
                  liftIO $ removeFile file
                _ -> do liftIO $ hPutStrLn output ("Module " ++ mod ++ " could not be removed.")
              return mod
          void $ reloadChangedModules (hPutStrLn output . ("Re-loaded module: " ++)) changedMods
        performChanges output True resMods = forM_ resMods (liftIO . \case 
          ContentChanged (n,m) -> do
            hPutStrLn output $ "### Module changed: " ++ n ++ "\n### new content:\n" ++ prettyPrint m
          ModuleRemoved mod ->
            hPutStrLn output $ "### Module removed: " ++ mod)

        getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods 


instance IsRefactSessionState CLISessionState where
  refSessMCs = refactState & _refSessMCs
  initSession = CLISessionState initSession Nothing False False
