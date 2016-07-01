{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.Refactor.OrganizeImports (organizeImports) where

import SrcLoc
import Name hiding (Name)
import GHC (Ghc, GhcMonad, lookupGlobalName, TyThing(..), moduleNameString, moduleName)
import qualified GHC
import TyCon
import ConLike
import DataCon
import Outputable (ppr, showSDocUnsafe)

import Control.Reference hiding (element)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function hiding ((&))
import Data.String
import Data.Maybe
import Data.Data
import Data.List
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase
import Debug.Trace

organizeImports :: forall n . (NamedThing n, Data n) => Ann Module (STWithNames n) -> RefactoredModule n
organizeImports mod
  = element&modImports&annListElems !~ narrowImports usedNames . sortImports $ mod
  where usedNames = map getName $ catMaybes
                                $ map (^? (annotation&semanticInfo&nameInfo))
                                $ (universeBi (mod ^. element&modHead) ++ universeBi (mod ^. element&modDecl) :: [Ann SimpleName (STWithNames n)])
        
-- | Sorts the imports in alphabetical order
sortImports :: [Ann ImportDecl (STWithNames n)] -> [Ann ImportDecl (STWithNames n)]
sortImports = sortBy (ordByOccurrence `on` (^. element&importModule&element))

-- | Modify an import to only import  names that are used.
narrowImports :: forall n . NamedThing n => [GHC.Name] -> [Ann ImportDecl (STWithNames n)] -> Refactor n [Ann ImportDecl (STWithNames n)]
narrowImports usedNames imps = foldM (narrowOneImport usedNames) imps imps 
  where narrowOneImport :: [GHC.Name] -> [Ann ImportDecl (STWithNames n)] -> Ann ImportDecl (STWithNames n) -> Refactor n [Ann ImportDecl (STWithNames n)]
        narrowOneImport names all one =
          (\case Just x -> map (\e -> if e == one then x else e) all
                 Nothing -> delete one all) <$> narrowImport names (map (^. semantics) all) one 
        
narrowImport :: NamedThing n => [GHC.Name] -> [SemanticInfo n] -> Ann ImportDecl (STWithNames n) 
                             -> Refactor n (Maybe (Ann ImportDecl (STWithNames n)))
narrowImport usedNames otherModules imp
  | importIsExact (imp ^. element) 
  = Just <$> (element&importSpec&annJust&element&importSpecList !~ narrowImportSpecs usedNames $ imp)
  | otherwise 
  = if null actuallyImported
      then if length (otherModules ^? traversal&importedModule&filtered (== importedMod) :: [GHC.Module]) > 1 
              then pure Nothing
              else Just <$> (element&importSpec !- replaceWithJust (mkImportSpecList []) $ imp)
      else pure (Just imp)
  where actuallyImported = map getName (fromJust (imp ^? annotation&semanticInfo&importedNames)) `intersect` usedNames
        Just importedMod = imp ^? annotation&semanticInfo&importedModule
    
-- | Narrows the import specification (explicitely imported elements)
narrowImportSpecs :: forall n . NamedThing n => [GHC.Name] -> AnnList IESpec (STWithNames n) -> Refactor n (AnnList IESpec (STWithNames n))
narrowImportSpecs usedNames 
  = (annList&element !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> IESpec (STWithNames n) -> Refactor n (IESpec (STWithNames n))
        narrowSpecSubspec usedNames spec 
          = do let Just specName = spec ^? ieName&element&simpleName&annotation&semanticInfo&nameInfo
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> map getName (tyConDataCons tc) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: Ann IESpec (STWithNames n) -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (ie ^? element&ieName&element&simpleName&annotation&semanticInfo&nameInfo) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? element&ieSubspec&annJust&element&essList&annList) /= [])
            || (case ie ^? element&ieSubspec&annJust&element of Just SubSpecAll -> True; _ -> False)     
  
narrowImportSubspecs :: NamedThing n => [GHC.Name] -> Ann SubSpec (STWithNames n) -> Ann SubSpec (STWithNames n)
narrowImportSubspecs [] (Ann _ SubSpecAll) = mkSubList []
narrowImportSubspecs _ ss@(Ann _ SubSpecAll) = ss
narrowImportSubspecs usedNames ss@(Ann _ (SubSpecList _)) 
  = element&essList .- filterList (\n -> fmap getName (n ^? element&simpleName&annotation&semanticInfo&nameInfo) `elem` map Just usedNames) $ ss
