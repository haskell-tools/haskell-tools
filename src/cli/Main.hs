{-# LANGUAGE LambdaCase
           , TupleSections 
           #-}
module Main where

import System.Environment
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

main :: IO ()
main = refactorSession =<< getArgs

type RefactorSession = StateT RefactorSessionState

refactorSession :: [String] -> IO ()
refactorSession args = runGhc (Just libdir) $ flip evalStateT initSession $
  do lift $ initGhcFlags
     workingDirs <- lift $ useFlags args
     lift $ useDirs workingDirs
     if null workingDirs then liftIO $ putStrLn "Usage: ht-refact [ghc-flags] package-pathes"
                         else do moduleNames <- initializeSession workingDirs
                                 runSession moduleNames
     
  where initializeSession :: [FilePath] -> RefactorSession Ghc [(FilePath, String)]
        initializeSession workingDirs = do
          moduleNames <- liftIO $ concat <$> mapM (\wd -> map (wd,) <$> getModules wd) workingDirs
          lift $ setTargets (map (\(_,mod) -> (Target (TargetModule (mkModuleName mod)) True Nothing)) moduleNames)
          liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
          lift $ load LoadAllTargets
          allMods <- lift getModuleGraph
          mods <- lift $ forM allMods (loadModule moduleNames)
          liftIO $ putStrLn "All modules loaded. Use 'SelectModule module-name' to select a module"
          modify $ refSessMods .= Map.fromList mods
          liftIO $ hSetBuffering stdout NoBuffering
          return moduleNames

        loadModule :: [(String, String)] -> ModSummary -> Ghc ((FilePath, String, IsBoot), TypedModule)
        loadModule moduleNames ms = 
          do mm <- parseTyped ms
             liftIO $ putStrLn ("Loaded module: " ++ (GHC.moduleNameString $ moduleName $ ms_mod ms))
             let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
                 Just wd = find ((modName ==) . snd) moduleNames 
             return ((fst wd, modName, case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot), mm)

        runSession :: [(String, String)] -> RefactorSession Ghc ()
        runSession moduleNames = do 
          actualMod <- gets (^. actualMod)
          liftIO $ putStr (maybe "no-module-selected" (\(_,m,_) -> m) actualMod ++ ">")
          cmd <- liftIO $ getLine 
          sessionComm <- readSessionCommand cmd
          performSessionCommand sessionComm
          doExit <- gets (^. exiting)
          when (not doExit) (runSession moduleNames)

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
       case res of Left err -> liftIO $ putStrLn err
                   Right resMods -> do 
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
  where getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods 

