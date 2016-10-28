{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.Predefined.OrganizeImports (organizeImports, OrganizeImportsDomain) where

import SrcLoc
import Name hiding (Name)
import GHC (Ghc, GhcMonad, lookupGlobalName, TyThing(..), moduleNameString, moduleName)
import qualified GHC
import TyCon
import ConLike
import DataCon
import Outputable (Outputable(..), ppr, showSDocUnsafe)

import Control.Reference hiding (element)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function hiding ((&))
import Data.String
import Data.Maybe
import Data.Data
import Data.List
import Data.Generics.Uniplate.Data

import Language.Haskell.Tools.Refactor as AST

type OrganizeImportsDomain dom = ( HasNameInfo dom, HasImportInfo dom )

organizeImports :: forall dom . OrganizeImportsDomain dom => LocalRefactoring dom
organizeImports mod
  = modImports&annListElems !~ narrowImports usedNames . sortImports $ mod
  where usedNames = map getName $ catMaybes $ map semanticsName
                        -- obviously we don't want the names in the imports to be considered, but both from
                        -- the declarations (used), both from the module head (re-exported) will count as usage
                      $ (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [QualifiedName dom])
        
-- | Sorts the imports in alphabetical order
sortImports :: [ImportDecl dom] -> [ImportDecl dom]
sortImports = sortBy (compare `on` (^. importModule&AST.moduleNameString))

-- | Modify an import to only import  names that are used.
narrowImports :: forall dom . OrganizeImportsDomain dom 
              => [GHC.Name] -> [ImportDecl dom] -> LocalRefactor dom [ImportDecl dom]
narrowImports usedNames imps = foldM (narrowOneImport usedNames) imps imps 
  where narrowOneImport :: [GHC.Name] -> [ImportDecl dom] -> ImportDecl dom -> LocalRefactor dom [ImportDecl dom]
        narrowOneImport names all one =
          (\case Just x -> map (\e -> if e == one then x else e) all
                 Nothing -> delete one all) <$> narrowImport names (map semanticsImportedModule all) one 

-- | Reduces the number of definitions used from an import
narrowImport :: OrganizeImportsDomain dom
             => [GHC.Name] -> [GHC.Module] -> ImportDecl dom 
                           -> LocalRefactor dom (Maybe (ImportDecl dom))
narrowImport usedNames otherModules imp
  | importIsExact imp
  = Just <$> (importSpec&annJust&importSpecList !~ narrowImportSpecs usedNames $ imp)
  | otherwise 
  = if null actuallyImported
      then if length (filter (== importedMod) otherModules) > 1 
              then pure Nothing
              else Just <$> (importSpec !- replaceWithJust (mkImportSpecList []) $ imp)
      else pure (Just imp)
  where actuallyImported = semanticsImported imp `intersect` usedNames
        importedMod = semanticsImportedModule imp
    
-- | Narrows the import specification (explicitely imported elements)
narrowImportSpecs :: forall dom . OrganizeImportsDomain dom
                  => [GHC.Name] -> IESpecList dom -> LocalRefactor dom (IESpecList dom)
narrowImportSpecs usedNames 
  = (annList !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> IESpec dom -> LocalRefactor dom (IESpec dom)
        narrowSpecSubspec usedNames spec 
          = do let Just specName = semanticsName =<< (spec ^? ieName&simpleName)
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> map getName (tyConDataCons tc) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: IESpec dom -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (semanticsName =<< (ie ^? ieName&simpleName)) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)     

-- | Reduces the number of definitions imported from a sub-specifier.
narrowImportSubspecs :: OrganizeImportsDomain dom => [GHC.Name] -> SubSpec dom -> SubSpec dom
narrowImportSubspecs [] SubAll = mkSubList []
narrowImportSubspecs _ ss@SubAll = ss
narrowImportSubspecs usedNames ss@(SubList {}) 
  = essList .- filterList (\n -> fmap getName (semanticsName =<< (n ^? simpleName)) `elem` map Just usedNames) $ ss
