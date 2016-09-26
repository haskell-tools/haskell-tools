{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.CLI (refactorSession) where

import System.Directory
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad.State
import Control.Reference

import GHC
import HscTypes as GHC
import Module as GHC
import GHC.Paths ( libdir )

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Session

tryOut = refactorSession [ "-dry-run", "-one-shot", "-module-name=Language.Haskell.Tools.AST", "-refactoring=OrganizeImports"
                         , "-package", "ghc", "src/ast", "src/ast-trf", "src/ast-ghc", "src/ast-ppr", "src/ast-gen", "src/refactor"]

refactorSession :: [String] -> IO ()
refactorSession args = runGhc (Just libdir) $ flip evalStateT initSession $
  do lift $ initGhcFlags
     workingDirsAndHtFlags <- lift $ useFlags args
     let (htFlags, workingDirs) = partition (\f -> head f == '-') workingDirsAndHtFlags
     lift $ useDirs workingDirs
     if null workingDirs then liftIO $ putStrLn usageMessage
                         else do moduleNames <- initializeSession workingDirs htFlags
                                 runSession moduleNames htFlags
     
  where initializeSession :: [FilePath] -> [String] -> RefactorSession Ghc [(FilePath, String)]
        initializeSession workingDirs flags = do
          moduleNames <- liftIO $ concat <$> mapM (\wd -> map (wd,) <$> getModules wd) workingDirs
          lift $ setTargets (map (\(_,mod) -> (Target (TargetModule (mkModuleName mod)) True Nothing)) moduleNames)
          liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
          lift $ load LoadAllTargets
          allMods <- lift getModuleGraph
          mods <- lift $ forM allMods (loadModule moduleNames)
          liftIO $ putStrLn "All modules loaded. Use 'SelectModule module-name' to select a module"
          modify $ refSessMods .= Map.fromList mods
          liftIO $ hSetBuffering stdout NoBuffering
          when ("-dry-run" `elem` flags) $ modify (dryMode .= True)
          return moduleNames

        loadModule :: [(String, String)] -> ModSummary -> Ghc ((FilePath, String, IsBoot), TypedModule)
        loadModule moduleNames ms = 
          do mm <- parseTyped ms
             liftIO $ putStrLn ("Loaded module: " ++ (GHC.moduleNameString $ moduleName $ ms_mod ms))
             let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
                 Just wd = find ((modName ==) . snd) moduleNames 
             return ((fst wd, modName, case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot), mm)

        runSession :: [(String, String)] -> [String] -> RefactorSession Ghc ()
        runSession moduleNames flags | "-one-shot" `elem` flags
          = let modName = catMaybes $ map (\f -> case splitOn "=" f of ["-module-name", mod] -> Just mod; _ -> Nothing) flags
                refactoring = catMaybes $ map (\f -> case splitOn "=" f of ["-refactoring", ref] -> Just ref; _ -> Nothing) flags
             in case (modName, refactoring) of 
                  ([modName],[refactoring]) ->
                    do performSessionCommand (LoadModule modName)
                       performSessionCommand =<< readSessionCommand (dropWhile (=='"') $ takeWhile (/='"') $ refactoring)
                  _ -> liftIO $ putStrLn usageMessage
        runSession moduleNames _ = runSessionLoop moduleNames

        runSessionLoop :: [(String, String)] -> RefactorSession Ghc ()
        runSessionLoop moduleNames = do 
          actualMod <- gets (^. actualMod)
          liftIO $ putStr (maybe "no-module-selected" (\(_,m,_) -> m) actualMod ++ ">")
          cmd <- liftIO $ getLine 
          sessionComm <- readSessionCommand cmd
          performSessionCommand sessionComm
          doExit <- gets (^. exiting)
          when (not doExit) (runSessionLoop moduleNames)

        usageMessage = "Usage: ht-refact [ht-flags, ghc-flags] package-pathes\n"
                         ++ "ht-flags: -dry-run -one-shot -module-name=modulename -refactoring=\"refactoring\""

data RefactorSessionCommand 
  = LoadModule String
  | Exit
  | RefactorCommand RefactorCommand

readSessionCommand :: Monad m => String -> RefactorSession m RefactorSessionCommand
readSessionCommand cmd = case splitOn " " cmd of 
    ["SelectModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    _ -> do actualMod <- gets (^. actualMod)
            case actualMod of Just (wd,m,_) -> return $ RefactorCommand $ readCommand (toFileName wd m) cmd
                              Nothing -> error "Set the actual module first"

performSessionCommand :: RefactorSessionCommand -> RefactorSession Ghc ()
performSessionCommand (LoadModule mod) = do fnd <- gets (find (\(_,m,hs) -> m == mod && hs == NormalHs) . Map.keys . (^. refSessMods))
                                            if isJust fnd then modify $ actualMod .= fnd
                                                          else liftIO $ putStrLn ("Cannot find module: " ++ mod)
performSessionCommand Exit = modify $ exiting .= True
performSessionCommand (RefactorCommand cmd) 
  = do RefactorSessionState { _refSessMods = mods, _actualMod = Just act@(_, mod, _) } <- get
       res <- lift $ performCommand cmd (mod, mods Map.! act) (map (\((_,m,_),mod) -> (m,mod)) $ Map.assocs (Map.delete act mods))
       inDryMode <- gets (^. dryMode)
       case res of Left err -> liftIO $ putStrLn err
                   Right resMods -> performChanges inDryMode resMods
                     
  where performChanges False resMods = do 
          mss <- forM resMods $ \case 
            ContentChanged (n,m) -> do
              let modName = semanticsModule $ m ^. semantics
              ms <- getModSummary modName (isBootModule $ m ^. semantics)
              let isBoot = case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot
              Just (workingDir,_,_) <- gets (find (\(_,m,b) -> m == n && b == isBoot) . Map.keys . (^. refSessMods))
              liftIO $ withBinaryFile ((case isBoot of NormalHs -> toFileName; IsHsBoot -> toBootFileName) workingDir n) 
                                      WriteMode (`hPutStr` prettyPrint m)
              return $ Just (n, workingDir, modName, isBoot)
            ModuleRemoved mod -> do
              Just (workingDir,_,_) <- gets (find (\(_,m,b) -> m == mod) . Map.keys . (^. refSessMods))
              liftIO $ removeFile (toFileName workingDir mod)
              modify $ refSessMods .- Map.delete (workingDir, mod, IsHsBoot) . Map.delete (workingDir, mod, NormalHs)
              return Nothing
          lift $ load LoadAllTargets
          forM_ (catMaybes mss) $ \(n, workingDir, modName, isBoot) -> do
              -- TODO: add target if module is added as a change
              ms <- getModSummary modName (isBoot == IsHsBoot)
              newm <- lift $ parseTyped ms
              modify $ refSessMods .- Map.insert (workingDir, n, isBoot) newm
              liftIO $ putStrLn ("Re-loaded module: " ++ n)
        performChanges True resMods = forM_ resMods $ liftIO . \case 
          ContentChanged (n,m) -> do
            putStrLn $ "### Module changed: " ++ n
            putStrLn $ "### new content:"
            putStrLn (prettyPrint m)
          ModuleRemoved mod ->
            putStrLn $ "### Module removed: " ++ mod

        getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods 
