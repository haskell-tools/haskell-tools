{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Tools.Refactor.Builtin.FindUnused where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import GHC
import Name
import Data.Set (Set, empty, (\\), toList, insert)
import Data.List (intercalate, groupBy)
import Data.Function (on)
import Control.Reference
import Control.Monad.State
import Debug.Trace (trace)
import Outputable (ppr, showSDocUnsafe)

data UnusedState = UnusedState { _defined :: Set GHC.Name
                               , _used :: Set GHC.Name
                               }

initState = UnusedState empty empty

makeReferences ''UnusedState

findUnusedRefactoring :: RefactoringChoice
findUnusedRefactoring = ProjectRefactoring "FindUnused" findUnused

findUnused :: ProjectRefactoring
findUnused mods = do let (mod', st) = flip runState initState $ mapM (biplateRef !~ recordName) (map snd mods)
                         unused = toList ((st ^. defined) \\ (st ^. used))
                     unusedDefs <- filterM (fmap not . isRecordName) unused
                     let groupedUnused = groupBy ((==) `on` nameModule_maybe) unusedDefs
                     liftIO $ putStrLn $ "Unused definitions:"
                     liftIO $ putStrLn
                            $ unlines
                            $ map (\ls -> (showSDocUnsafe $ ppr $ nameModule_maybe (head ls)) ++ ": "
                                             ++ intercalate ", " (map (showSDocUnsafe . ppr) ls)) 
                            $ groupedUnused
                     return [] -- no changes
  where isRecordName n = do tt <- lookupName n
                            case tt of Just (AnId id) -> return $ isRecordSelector id
                                       _ -> return False

recordName :: QualifiedName -> State UnusedState QualifiedName
recordName n = do let name = semanticsName n
                      isDefined = semanticsDefining n 
                  case name of Just semaName -> if isDefined then modify $ defined .- insert semaName
                                                             else modify $ used .- insert semaName
                               Nothing -> return ()
                  return $ trace ("\n### Name: " ++ prettyPrint n ++ " " ++ show isDefined) $ n
