{-# LANGUAGE LambdaCase
           , MultiWayIf
           , TypeApplications
           , TypeFamilies
           , RecordWildCards
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Daemon.Update where

import Control.Exception (Exception(..))
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import qualified Data.ByteString.Char8 as StrictBS (unpack, readFile)
import Data.Either (Either(..), either, rights)
import Data.IORef (newIORef)
import Data.List hiding (insert)
import qualified Data.Map as Map (insert, keys, filter)
import Data.Maybe
import Data.Version (Version(..))
import System.Directory (setCurrentDirectory, removeFile, doesDirectoryExist)
import System.IO
import System.IO.Strict as StrictIO (hGetContents)

import Bag (bagToList)
import DynFlags (DynFlags(..), PkgConfRef(..))
import ErrUtils (ErrMsg(..))
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), modifySession)
import HscTypes (hsc_mod_graph)
import Packages (Version(..), initPackages)

import Language.Haskell.Tools.Daemon.PackageDB (packageDBLoc, detectAutogen)
import Language.Haskell.Tools.Daemon.Protocol (UndoRefactor(..), ResponseMsg(..), ClientMessage(..))
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.Session
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Paths_haskell_tools_daemon (version)

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: [RefactoringChoice IdDom] ->  (ResponseMsg -> IO ()) -> ClientMessage -> StateT DaemonSessionState Ghc Bool
updateClient _ resp (Handshake _) = liftIO (resp $ HandshakeResponse $ versionBranch version) >> return True
updateClient _ resp KeepAlive = liftIO (resp KeepAliveResponse) >> return True
updateClient _ resp Disconnect = liftIO (resp Disconnected) >> return False
updateClient _ _ (SetPackageDB pkgDB) = modify (packageDB .= pkgDB) >> return True
updateClient _ resp (AddPackages packagePathes) = do
    addPackages resp packagePathes
    return True
updateClient _ _ (SetWorkingDir fp) = liftIO (setCurrentDirectory fp) >> return True
updateClient _ resp (SetGHCFlags flags) = do (unused, change) <- lift (useFlags flags)
                                             liftIO $ resp $ UnusedFlags unused
                                             modify $ ghcFlagsSet .= change
                                             return True
updateClient _ _ (RemovePackages packagePathes) = do
    mcs <- gets (^. refSessMCs)
    let existingFiles = concatMap @[] (map (^. sfkFileName) . Map.keys) (mcs ^? traversal & filtered isRemoved & mcModules)
    lift $ forM_ existingFiles (\fs -> removeTarget (TargetFile fs Nothing))
    lift $ deregisterDirs (mcs ^? traversal & filtered isRemoved & mcSourceDirs & traversal)
    modify $ refSessMCs .- filter (not . isRemoved)
    modifySession (\s -> s { hsc_mod_graph = filter ((`notElem` existingFiles) . getModSumOrig) (hsc_mod_graph s) })
    mcs <- gets (^. refSessMCs)
    when (null mcs) $ modify (packageDBSet .= False)
    return True
  where isRemoved mc = (mc ^. mcRoot) `elem` packagePathes

updateClient _ resp (ReLoad added changed removed) =
  -- TODO: check for changed cabal files and reload their packages
  do lift $ forM_ removed (\src -> removeTarget (TargetFile src Nothing))
     -- remove targets deleted
     modify $ refSessMCs & traversal & mcModules
                .- Map.filter (\m -> maybe True ((`notElem` removed) . getModSumOrig) (m ^? modRecMS))
     modifySession (\s -> s { hsc_mod_graph = filter (\mod -> getModSumOrig mod `notElem` removed) (hsc_mod_graph s) })
     -- reload changed modules
     -- TODO: filter those that are in reloaded packages
     reloadRes <- reloadChangedModules (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]))
                                       (\mss -> resp (LoadingModules (map getModSumOrig mss)))
                                       (\ms -> getModSumOrig ms `elem` changed)
     mcs <- gets (^. refSessMCs)
     let mcsToReload = filter (\mc -> any ((mc ^. mcRoot) `isPrefixOf`) added && isNothing (moduleCollectionPkgId (mc ^. mcId))) mcs
     addPackages resp (map (^. mcRoot) mcsToReload) -- reload packages containing added modules
     liftIO $ case reloadRes of Left errs -> resp (either ErrorMessage CompilationProblem (getProblems errs))
                                Right _ -> return ()
     return True

updateClient _ _ Stop = modify (exiting .= True) >> return False

-- TODO: perform refactorings without selected modules
updateClient refactorings resp (PerformRefactoring refact modPath selection args shutdown) = do
    (selectedMod, otherMods) <- getFileMods modPath
    performRefactoring (refact:selection:args)
                       (maybe (Left modPath) Right selectedMod) otherMods
    when shutdown $ liftIO $ resp Disconnected
    return (not shutdown)
  where performRefactoring cmd actualMod otherMods = do
          res <- lift $ performCommand refactorings cmd actualMod otherMods
          case res of
            Left err -> liftIO $ resp $ ErrorMessage err
            Right diff -> do changedMods <- applyChanges diff
                             liftIO $ resp $ ModulesChanged (map (either id (\(_,_,ch) -> ch)) changedMods)
                             isWatching <- gets (isJust . (^. watchProc))
                             when (not isWatching && not shutdown)
                                 -- if watch is on, then it will automatically
                                 -- reload changed files, otherwise we do it manually
                               $ void $ reloadChanges (map ((^. sfkModuleName) . (\(key,_,_) -> key)) (rights changedMods))
        applyChanges changes = do
          forM changes $ \case
            ModuleCreated n m otherM -> do
              mcs <- gets (^. refSessMCs)
              Just (_, otherMR) <- gets (lookupModInSCs otherM . (^. refSessMCs))

              let Just otherMS = otherMR ^? modRecMS
                  Just mc = lookupModuleColl (otherM ^. sfkModuleName) mcs
              otherSrcDir <- liftIO $ getSourceDir otherMS
              let loc = toFileName otherSrcDir n
              modify $ refSessMCs & traversal & filtered (\mc' -> (mc' ^. mcId) == (mc ^. mcId)) & mcModules
                         .- Map.insert (SourceFileKey loc n) (ModuleNotLoaded False False)
              liftIO $ withBinaryFile loc WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle (prettyPrint m)
              lift $ addTarget (Target (TargetFile loc Nothing) True Nothing)
              return $ Right (SourceFileKey loc n, loc, RemoveAdded loc)
            ContentChanged (n,m) -> do
              let newCont = prettyPrint m
                  file = n ^. sfkFileName
              origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
                hSetEncoding handle utf8
                StrictIO.hGetContents handle
              let undo = createUndo 0 $ getGroupedDiff origCont newCont
              liftIO $ withBinaryFile file WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle newCont
              return $ Right (n, file, UndoChanges file undo)
            ModuleRemoved mod -> do
              Just (sfk,_) <- gets (lookupModuleInSCs mod . (^. refSessMCs))
              let file = sfk ^. sfkFileName
              origCont <- liftIO (StrictBS.unpack <$> StrictBS.readFile file)
              lift $ removeTarget (TargetFile file Nothing)
              modify $ (refSessMCs .- removeModule mod)
              liftIO $ removeFile file
              return $ Left $ RestoreRemoved file origCont

        reloadChanges changedMods
          = do reloadRes <- reloadChangedModules (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]))
                                                 (\mss -> resp (LoadingModules (map getModSumOrig mss)))
                                                 (\ms -> getModSumName ms `elem` changedMods)
               liftIO $ case reloadRes of Left errs -> resp (either ErrorMessage (ErrorMessage . ("The result of the refactoring contains errors: " ++) . show) (getProblems errs))
                                          Right _ -> return ()

addPackages :: (ResponseMsg -> IO ()) -> [FilePath] -> StateT DaemonSessionState Ghc ()
addPackages _ [] = return ()
addPackages resp packagePathes = do
  nonExisting <- filterM ((return . not) <=< liftIO . doesDirectoryExist) packagePathes
  forM_ packagePathes watchNew -- put a file system watch on each package
  DaemonSessionState {..} <- get
  if (not (null nonExisting))
    then liftIO $ resp $ ErrorMessage $ "The following packages are not found: " ++ concat (intersperse ", " nonExisting)
    else do
      -- clear existing removed packages
      existingMCs <- gets (^. refSessMCs)
      let existing = (existingMCs ^? traversal & filtered isTheAdded & mcModules & traversal & modRecMS)
          existingModNames = map ms_mod existing
      needToReload <- handleErrors $ (filter (\ms -> not $ ms_mod ms `elem` existingModNames))
                                       <$> getReachableModules (\_ -> return ()) (\ms -> ms_mod ms `elem` existingModNames)
      modify $ refSessMCs .- filter (not . isTheAdded) -- remove the added package from the database
      forM_ existing $ \ms -> removeTarget (TargetFile (getModSumOrig ms) Nothing)
      modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existingModNames) . ms_mod) (hsc_mod_graph s) })
      -- load new modules
      pkgDBok <- initializePackageDBIfNeeded
      if pkgDBok then do
        res <- loadPackagesFrom
                 (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)])
                           >> return (getModSumOrig ms))
                 (resp . LoadingModules . map getModSumOrig)
                 (\st fp -> maybeToList <$> detectAutogen fp (st ^. packageDB)) packagePathes
        case res of
          Right _ -> do
            mapM_ (reloadModule (\_ -> return ())) (either (const []) id needToReload) -- don't report consequent reloads (not expected)
          Left err -> liftIO $ resp $ either ErrorMessage CompilationProblem (getProblems err)
      else liftIO $ resp $ ErrorMessage $ "Attempted to load two packages with different package DB. "
                                            ++ "Stack, cabal-sandbox and normal packages cannot be combined"
  where isTheAdded mc = (mc ^. mcRoot) `elem` packagePathes
        initializePackageDBIfNeeded = do
          pkgDBAlreadySet <- gets (^. packageDBSet)
          pkgDB <- gets (^. packageDB)
          locs <- liftIO $ mapM (packageDBLoc pkgDB) packagePathes
          case locs of
            firstLoc:rest ->
              if | not (all (== firstLoc) rest)
                     -> return False
                 | pkgDBAlreadySet -> do
                     pkgDBLocs <- gets (^. packageDBLocs)
                     return (pkgDBLocs == firstLoc)
                 | otherwise -> do
                     usePackageDB firstLoc
                     modify ((packageDBSet .= True) . (packageDBLocs .= firstLoc))
                     return True
            [] -> return True

createUndo :: Eq a => Int -> [Diff [a]] -> [(Int, Int, [a])]
createUndo i (Both str _ : rest) = createUndo (i + length str) rest
createUndo i (First rem : Second add : rest)
  = (i, i + length add, rem) : createUndo (i + length add) rest
createUndo i (First rem : rest) = (i, i, rem) : createUndo i rest
createUndo i (Second add : rest)
  = (i, i + length add, []) : createUndo (i + length add) rest
createUndo _ [] = []

initGhcSession :: IO Session
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags >> getSession))

usePackageDB :: GhcMonad m => [FilePath] -> m ()
usePackageDB [] = return ()
usePackageDB pkgDbLocs
  = do dfs <- getSessionDynFlags
       dfs' <- liftIO $ fmap fst $ initPackages
                 $ dfs { extraPkgConfs = (map PkgConfFile pkgDbLocs ++) . extraPkgConfs dfs
                       , pkgDatabase = Nothing
                       }
       void $ setSessionDynFlags dfs'

getProblems :: RefactorException -> Either String [(SrcSpan, String)]
getProblems (SourceCodeProblem errs) = Right $ map (\err -> (errMsgSpan err, show err)) $ bagToList errs
getProblems other = Left $ displayException other

watchNew :: FilePath -> StateT DaemonSessionState Ghc ()
watchNew fp = do
    watch <- gets (^. watchProc)
    case watch of Just w -> void $ liftIO $ hPutStrLn (_watchStdIn w) $ "watch " ++ fp
                  _      -> return ()
