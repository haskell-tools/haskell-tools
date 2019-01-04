{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Resolves how the daemon should react to individual requests from the client.
module Language.Haskell.Tools.Daemon.Update (updateClient, updateForFileChanges, initGhcSession) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import qualified Data.ByteString.Char8 as StrictBS (unpack, readFile)
import Data.Either (Either(..), either, rights)
import Control.Concurrent.MVar
import Data.IORef (readIORef, newIORef)
import Data.List as List hiding (insert)
import qualified Data.Map as Map (insert, keys, filter)
import Data.Maybe
import qualified Data.Set as Set (fromList, isSubsetOf, (\\))
import Data.Version (Version(..))
import System.Directory
import System.FSWatch.Slave (watch)
import System.FilePath (FilePath, takeDirectory, takeExtension)
import System.IO
import System.IO.Strict as StrictIO (hGetContents)
import Text.PrettyPrint as PP (text, render)

import DynFlags
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), modifySession)
import HscTypes
import Outputable
import Language.Haskell.Tools.Daemon.ErrorHandling (getProblems)
import Language.Haskell.Tools.Daemon.Options (SharedDaemonOptions(..), DaemonOptions(..))
import Language.Haskell.Tools.Daemon.PackageDB (decidePkgDB, packageDBLoc, detectAutogen)
import Language.Haskell.Tools.Daemon.Protocol as HT
import Language.Haskell.Tools.Daemon.Representation as HT
import Language.Haskell.Tools.Daemon.Session
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Linker (unload)
import Packages (initPackages)
import Paths_haskell_tools_daemon (version)

-- | Context for responding to a user request.
data UpdateCtx = UpdateCtx { options :: DaemonOptions
                           , refactorings :: [RefactoringChoice]
                           , queries :: [QueryChoice]
                           , response :: ResponseMsg -> IO ()
                           , warnMVar :: MVar [Marker]
                           }

-- | This function does the real job of acting upon client messages in a stateful environment of a
-- client.
updateClient :: DaemonOptions -> MVar [Marker] -> [RefactoringChoice] -> [QueryChoice]
                  -> (ResponseMsg -> IO ()) -> ClientMessage
                  -> DaemonSession Bool
updateClient options warnMVar refactors queries resp
  = updateClient' (UpdateCtx options refactors queries resp warnMVar)

updateClient' :: UpdateCtx -> ClientMessage -> DaemonSession Bool
-- resets the internal state of Haskell-tools (but keeps options)
updateClient' UpdateCtx{..} Reset
  = do roots <- gets (^? refSessMCs & traversal & mcRoot)
       modify' $ resetSession
       Session sess <- liftIO $ reinitGhcSession warnMVar (generateCode (sharedOptions options))
       env <- liftIO $ readIORef sess
       dfs <- getSessionDynFlags
       when (not $ gopt Opt_ExternalInterpreter dfs) $
         liftIO $ unload env [] -- clear (unload everything) the global state of the linker
       lift $ setSession env
       addPackages response warnMVar roots
       return True

updateClient' UpdateCtx{..} (Handshake _)
  = do liftIO (response $ HandshakeResponse $ versionBranch version)
       return True

updateClient' UpdateCtx{..} KeepAlive
  = do liftIO (response KeepAliveResponse)
       return True

updateClient' UpdateCtx{..} Disconnect
  = do liftIO (response Disconnected)
       return False

updateClient' UpdateCtx{..} (SetPackageDB pkgDB)
  = do mcs <- gets (^. refSessMCs)
       if null mcs
         then modify' (packageDB .= Just (pkgDB, True))
         else liftIO $ response $ ErrorMessage "The package database is already in use and cannot be changed."
       return True

updateClient' UpdateCtx{..} (AddPackages packagePathes)
  = do addPackages response warnMVar packagePathes
       return True

updateClient' _ (SetWorkingDir fp)
  = do liftIO (setCurrentDirectory fp)
       return True

updateClient' UpdateCtx{..} (SetGHCFlags flags)
  = do (unused, change) <- lift (useFlags flags)
       when (not $ null unused) $ liftIO $ response $ UnusedFlags unused
       modify' $ ghcFlagsSet .= change
       return True

updateClient' UpdateCtx{..} (RemovePackages packagePathes) = do
    mcs <- gets (^. refSessMCs)
    let existingFiles = concatMap @[] (map (^. sfkFileName) . Map.keys) (mcs ^? traversal & filtered isRemoved & mcModules)
    lift $ forM_ existingFiles (\fs -> removeTarget (TargetFile fs Nothing))
    lift $ deregisterDirs (mcs ^? traversal & filtered isRemoved & mcSourceDirs & traversal)
    modify' $ refSessMCs .- filter (not . isRemoved)
    modifySession (\s -> s { hsc_mod_graph = mkModuleGraph $ filter ((`notElem` existingFiles) . getModSumOrig) (mgModSummaries $ hsc_mod_graph s) })
    mcs <- gets (^. refSessMCs)
    when (null mcs) $ modify' (packageDB .= Nothing)
    return True
  where isRemoved mc = (mc ^. mcRoot) `elem` packagePathes

updateClient' UpdateCtx{..} UndoLast | disableHistory $ sharedOptions options
  = do liftIO $ response $ ErrorMessage "Recording history has been disabled from command line."
       return True
updateClient' UpdateCtx{..} UndoLast =
  do undos <- gets (^. undoStack)
     case undos of
       [] -> do liftIO $ response $ ErrorMessage "There is nothing to undo. Please note that the refactoring history is cleared after manual changes in the refactored files."
                return True
       lastUndo:_ -> do
         modify (undoStack .- tail)
         liftIO $ mapM_ performUndo lastUndo
         reloadModules response warnMVar (getUndoAdded lastUndo)
                                         (getUndoChanged lastUndo)
                                         (getUndoRemoved lastUndo) -- reload the reverted files
         return True

updateClient' UpdateCtx{..} (ReLoad added changed removed)
  = do updateForFileChanges response warnMVar added changed removed
       return True

updateClient' _ Stop
  = do modify (exiting .= True)
       return False

updateClient' UpdateCtx{..} (PerformQuery query modPath selection args shutdown)
  = do (selectedMod, otherMods) <- getFileMods modPath
       res <- lift $ performQuery queries (query:selection:args) (maybe (Left modPath) Right selectedMod) otherMods
       case res of
         Left err -> liftIO $ response $ ErrorMessage err
         Right (qType, qRes) -> liftIO $ response $ QueryResult query qType qRes
       when shutdown $ liftIO $ response Disconnected
       return (not shutdown)

updateClient' UpdateCtx{..} (PerformRefactoring refact modPath selection args shutdown diffMode)
  = do (selectedMod, otherMods) <- getFileMods modPath
       performRefactoring (refact:selection:args)
                          (maybe (Left modPath) Right selectedMod) otherMods
       when shutdown $ liftIO $ response Disconnected
       return (not shutdown)
  where performRefactoring cmd actualMod otherMods = do
          res <- lift $ performCommand refactorings cmd actualMod otherMods
          case res of
            Left err -> liftIO $ response $ ErrorMessage err
            Right diff -> do changedMods <- applyChanges diff
                             if not diffMode
                               then when (not (disableHistory $ sharedOptions options)) $ updateHistory changedMods
                               else liftIO $ response $ DiffInfo (concatMap (either snd (^. _4)) changedMods)
                             isWatching <- gets (isJust . (^. watchProc))
                             if not isWatching && not shutdown && not diffMode
                              -- if watch is on, then it will automatically
                              -- reload changed files, otherwise we do it manually
                               then do reloadChanges (map ((^. sfkFileName) . (^. _1)) (rights changedMods))
                                       reportWarnings response warnMVar
                               else modify (touchedFiles .= Set.fromList (map ((^. sfkFileName) . (^. _1)) (rights changedMods)))

        applyChanges changes = do
          forM changes $ \case
            ModuleCreated n m otherM -> do
              mcs <- gets (^. refSessMCs)
              otherMR <- gets (lookupModInSCs otherM . (^. refSessMCs))
              let Just otherMS = otherMR ^? just & _2 & modRecMS
                  Just mc = lookupModuleColl (otherM ^. sfkModuleName) mcs
              otherSrcDir <- liftIO $ getSourceDir otherMS
              let loc = toFileName otherSrcDir n
              let newCont = prettyPrint m
              when (not diffMode) $ do
                modify' $ refSessMCs & traversal & filtered (\mc' -> (mc' ^. mcId) == (mc ^. mcId)) & mcModules
                            .- Map.insert (SourceFileKey loc n) (ModuleNotLoaded NoCodeGen False)
                liftIO $ withBinaryFile loc WriteMode $ \handle -> do
                  hSetEncoding handle utf8
                  hPutStr handle newCont
                  hFlush handle
                lift $ addTarget (Target (TargetFile loc Nothing) True Nothing)
              return $ Right (SourceFileKey loc n, loc, RemoveAdded loc, createUnifiedDiff loc "" newCont)
            ContentChanged (n,m) -> do
              let newCont = prettyPrint m
                  file = n ^. sfkFileName
              origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
                hSetEncoding handle utf8
                StrictIO.hGetContents handle
              let undo = createUndo 0 $ getGroupedDiff origCont newCont
              let unifiedDiff = createUnifiedDiff file origCont newCont
              when (not diffMode) $ do
               liftIO $ withBinaryFile file WriteMode $ \handle -> do
                 hSetEncoding handle utf8
                 hPutStr handle newCont
                 hFlush handle
              return $ Right (n, file, UndoChanges file undo, unifiedDiff)
            ModuleRemoved mod -> do
              sfk <- gets (lookupModuleInSCs mod . (^. refSessMCs))
              let Just file = sfk ^? just & _1 & sfkFileName
              origCont <- liftIO (StrictBS.unpack <$> StrictBS.readFile file)
              when (not diffMode) $ do
                lift $ removeTarget (TargetFile file Nothing)
                modify' $ (refSessMCs .- removeModule mod)
                liftIO $ removeFile file
              return $ Left (RestoreRemoved file origCont, createUnifiedDiff file origCont "")

        reloadChanges changedMods
          = reloadChangedModules (\ms -> response (LoadedModule (getModSumOrig ms) (getModSumName ms)))
                                 (\mss -> response (LoadingModules (map getModSumOrig mss)))
                                 (\ms -> getModSumOrig ms `elem` changedMods)

        updateHistory :: [Either (UndoRefactor, b) (SourceFileKey, FilePath, UndoRefactor, String)] -> DaemonSession ()
        updateHistory changedMods
          = do modify (undoStack .- (map (either fst (^. _3)) changedMods :))
               -- force the evaluation of the undo stack to prevent older versions of
               -- modules seeming to be used when they could be garbage collected
               us <- gets (^. undoStack)
               liftIO $ evaluate $ force us
               return ()

addPackages :: (ResponseMsg -> IO ()) -> MVar [Marker] -> [FilePath] -> DaemonSession ()
addPackages _ _ [] = return ()
addPackages resp warnMVar packagePathes = do
  roots <- liftIO $ mapM canonicalizePath packagePathes
  nonExisting <- filterM ((return . not) <=< liftIO . doesDirectoryExist) roots
  DaemonSessionState {..} <- get
  if (not (null nonExisting))
    then liftIO $ resp $ ErrorMessage $ "The following packages are not found: " ++ concat (intersperse ", " nonExisting)
    else do
      forM_ roots watchNew -- put a file system watch on each package
      -- clear existing removed packages
      existingMCs <- gets (^. refSessMCs)
      let existing = (existingMCs ^? traversal & filtered (isTheAdded roots) & mcModules & traversal & modRecMS)
          existingModNames = map ms_mod existing
      needToReload <- (filter (\ms -> not $ ms_mod ms `elem` existingModNames))
                        <$> getReachableModules (\_ -> return ()) (\ms -> ms_mod ms `elem` existingModNames)
      modify' $ refSessMCs .- filter (not . isTheAdded roots) -- remove the added package from the database
      forM_ existing $ \ms -> removeTarget (TargetFile (getModSumOrig ms) Nothing)
      modifySession (\s -> s { hsc_mod_graph = mkModuleGraph $ filter (not . (`elem` existingModNames) . ms_mod) (mgModSummaries $ hsc_mod_graph s) })
      -- load new modules
      pkgDBok <- initializePackageDBIfNeeded roots
      if pkgDBok then do
        errs <- loadPackagesFrom
                  (\ms -> resp (LoadedModule (getModSumOrig ms) (getModSumName ms)))
                  (resp . LoadingModules . map getModSumOrig)
                  (\st fp -> maybe (return []) (fmap maybeToList . detectAutogen fp . fst) (st ^. packageDB)) roots
        -- handle source errors here to prevent rollback on the tool state
        let errors = foldl mappend ([],[]) (map getProblems errs)
        if null (fst errors)
          then do mapM_ (reloadModule (\_ -> return ())) needToReload -- don't report consequent reloads (not expected)
                  reportWarnings resp warnMVar
          else liftIO $ resp $ uncurry CompilationProblem errors
      else liftIO $ resp $ ErrorMessage $ "Attempted to load two packages with different package DB. "
                                            ++ "Stack, cabal-sandbox and normal packages cannot be combined"
  where isTheAdded roots mc = (mc ^. mcRoot) `elem` roots
        initializePackageDBIfNeeded roots = do
          db <- dbSetup roots
          case db of
            Nothing -> return False
            Just (pkgDB, _) -> do
              locs <- liftIO $ packageDBLoc pkgDB (head roots)
              usePackageDB locs
              modify' (packageDBLocs .= locs)
              return True
        dbSetup roots = do
          pkgDB <- gets (^. packageDB)
          case pkgDB of Nothing          -> do db <- liftIO $ decidePkgDB roots
                                               modify (packageDB .= fmap (, False) db)
                                               return (fmap (, False) db)
                        Just (_, True)   -> return pkgDB
                        Just (db, False) -> do newDB <- liftIO $ decidePkgDB roots
                                               if newDB == Just db then return pkgDB
                                                                   else return Nothing

-- | Updates the state of the tool after some files have been changed (possibly by another application)
updateForFileChanges :: (ResponseMsg -> IO ()) -> MVar [Marker] -> [FilePath] -> [FilePath] -> [FilePath] -> DaemonSession ()
updateForFileChanges resp warnMVar added changed removed = do
  -- clear undo stack if the changes are from another application
  refactoredFiles <- gets (^. touchedFiles)
  let changeSet = Set.fromList (added ++ changed ++ removed)
  when (not $ changeSet `Set.isSubsetOf` refactoredFiles)
    $ modify (undoStack .= []) -- clear the undo stack, if this changeset is not a result of a refactoring
  -- check for module collections that failed to load
  mcs <- gets (^. refSessMCs)
  let packagesNotLoaded = filter (not . (^. mcLoadDone)) mcs
      tryLoadAgain = filter (\r -> List.any (r `isPrefixOf`) (added ++ changed ++ removed)) $ map (^. mcRoot) packagesNotLoaded
  -- check for changes in .cabal files
  modify (touchedFiles .- (Set.\\ changeSet))
  let changedPackages = map takeDirectory
                          $ filter (\fp -> takeExtension fp == ".cabal")
                          $ added ++ changed ++ removed
      reloadedPackages = tryLoadAgain ++ changedPackages
      packageNotReloaded fp = not $ any (`isPrefixOf` fp) reloadedPackages
  addPackages resp warnMVar reloadedPackages -- reload packages if needed
  -- reload the rest of the modules
  reloadModules resp warnMVar (filter packageNotReloaded added) (filter packageNotReloaded changed)
                              (filter packageNotReloaded removed)

-- | Reloads changed modules to have an up-to-date version loaded
reloadModules :: (ResponseMsg -> IO ()) -> MVar [Marker] -> [FilePath] -> [FilePath] -> [FilePath] -> DaemonSession ()
reloadModules _ _ [] [] [] = return ()
reloadModules resp warnMVar added changed removed = do
  lift $ forM_ removed (\src -> removeTarget (TargetFile src Nothing))
  -- remove targets deleted
  modify' $ refSessMCs & traversal & mcModules
              .- Map.filter (\m -> maybe True ((`notElem` removed) . getModSumOrig) (m ^? modRecMS))
  modifySession (\s -> s { hsc_mod_graph = mkModuleGraph $ filter (\mod -> getModSumOrig mod `notElem` removed) (mgModSummaries $ hsc_mod_graph s) })
  -- reload changed modules
  -- TODO: filter those that are in reloaded packages
  reloadChangedModules (\ms -> resp (LoadedModule (getModSumOrig ms) (getModSumName ms)))
                       (\mss -> resp (LoadingModules (map getModSumOrig mss)))
                       (\ms -> getModSumOrig ms `elem` changed)
  reportWarnings resp warnMVar
  mcs <- gets (^. refSessMCs)
  let mcsToReload = filter (\mc -> any ((mc ^. mcRoot) `isPrefixOf`) added && isNothing (moduleCollectionPkgId (mc ^. mcId))) mcs
  addPackages resp warnMVar (map (^. mcRoot) mcsToReload) -- reload packages containing added modules

reportWarnings :: (ResponseMsg -> IO ()) -> MVar [Marker] -> DaemonSession ()
reportWarnings resp warnMVar = liftIO $ do
  warns <- modifyMVar warnMVar (return . ([],))
  when (not (null warns)) $ (resp $ CompilationProblem warns [])

-- | Creates a compressed set of changes in one file
createUndo :: Eq a => Int -> [Diff [a]] -> [(Int, Int, [a])]
createUndo i (Both str _ : rest) = createUndo (i + length str) rest
createUndo i (First rem : Second add : rest)
  = (i, i + length add, rem) : createUndo (i + length add) rest
createUndo i (First rem : rest) = (i, i, rem) : createUndo i rest
createUndo i (Second add : rest)
  = (i, i + length add, []) : createUndo (i + length add) rest
createUndo _ [] = []

-- | Creates a unified-style diff of two texts. Only used when the user wants to know what would change.
createUnifiedDiff :: FilePath -> String -> String -> String
createUnifiedDiff name left right
  = render $ prettyContextDiff (PP.text name) (PP.text name) PP.text $ getContextDiff 3 (lines left) (lines right)

-- | Undo a refactoring change using the information that was saved earlier.
performUndo :: UndoRefactor -> IO ()
performUndo (RemoveAdded fp) = removeFile fp
performUndo (RestoreRemoved fp cont)
  = liftIO $ withBinaryFile fp WriteMode $ \handle -> do
      hSetEncoding handle utf8
      hPutStr handle cont
performUndo (UndoChanges fp changes) = do
  cont <- liftIO $ withBinaryFile fp ReadMode $ \handle -> do
    hSetEncoding handle utf8
    StrictIO.hGetContents handle
  liftIO $ withBinaryFile fp WriteMode $ \handle -> do
      hSetEncoding handle utf8
      hPutStr handle (performUndoChanges 0 changes cont)

-- | Undo the changes in one file using the information that was saved earlier. See 'createUndo'.
performUndoChanges :: Int -> FileDiff -> String -> String
performUndoChanges i ((start,end,replace):rest) str | i == start
  = replace ++ performUndoChanges end rest (drop (end-start) str)
performUndoChanges i diffs (c:str) = c : performUndoChanges (i+1) diffs str
performUndoChanges _ _ [] = []

-- | Get the files added by a refactoring.
getUndoAdded :: [UndoRefactor] -> [FilePath]
getUndoAdded = catMaybes . map (\case RestoreRemoved fp _ -> Just fp
                                      _                   -> Nothing)

-- | Get the files changed by a refactoring.
getUndoChanged :: [UndoRefactor] -> [FilePath]
getUndoChanged = catMaybes . map (\case UndoChanges fp _ -> Just fp
                                        _                -> Nothing)

-- | Get the files removed by a refactoring.
getUndoRemoved :: [UndoRefactor] -> [FilePath]
getUndoRemoved = catMaybes . map (\case RemoveAdded fp -> Just fp
                                        _              -> Nothing)

initGhcSession :: Bool -> IO (Session, MVar [Marker])
initGhcSession genCode = do
  mv <- newMVar []
  sess <- Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags' genCode True >> setupLogging mv >> getSession))
  return (sess, mv)

reinitGhcSession :: MVar [Marker] -> Bool -> IO Session
reinitGhcSession mv genCode = do
  Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags' genCode True >> setupLogging mv >> getSession))

setupLogging :: MVar [Marker] -> Ghc ()
setupLogging mv = modifySession $ \s -> s { hsc_dflags = (hsc_dflags s) { log_action = logger } }
  where logger dfs reason sev sp _ msg = modifyMVar_ mv (return . (Marker sp (severity sev reason) (showSDoc dfs msg) :))
        severity SevError _ = Error
        severity SevWarning (Reason Opt_WarnDeferredTypeErrors) = Error
        severity SevWarning (Reason Opt_WarnDeferredOutOfScopeVariables) = Error
        severity SevWarning _ = HT.Warning
        severity _ _ = Info

usePackageDB :: GhcMonad m => [FilePath] -> m ()
usePackageDB [] = return ()
usePackageDB pkgDbLocs
  = do dfs <- getSessionDynFlags
       dfs' <- liftIO $ fmap fst $ initPackages
                 $ dfs { packageDBFlags = map (PackageDB . PkgConfFile) pkgDbLocs ++ packageDBFlags dfs
                       , pkgDatabase = Nothing
                       }
       void $ setSessionDynFlags dfs'

watchNew :: FilePath -> DaemonSession ()
watchNew fp = do
    wt <- gets (^. watchProc)
    maybe (return ()) (\w -> watch w fp) wt
