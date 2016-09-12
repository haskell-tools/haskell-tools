{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.OrganizeImports (organizeImports, OrganizeImportsDomain) where

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
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase
import Debug.Trace

type OrganizeImportsDomain dom n = ( Domain dom, HasNameInfo (SemanticInfo' dom SameInfoNameCls)
                                   , SemanticInfo' dom SameInfoImportCls ~ ImportInfo n, NamedThing n )

organizeImports :: forall n dom . OrganizeImportsDomain dom n => LocalRefactoring dom
organizeImports mod
  = element&modImports&annListElems !~ narrowImports usedNames . sortImports $ mod
  where usedNames = map getName $ catMaybes
                                $ map (semanticsName . (^. (annotation&semanticInfo)))
                                $ (universeBi (mod ^. element&modHead) ++ universeBi (mod ^. element&modDecl) :: [Ann QualifiedName dom SrcTemplateStage])
        
-- | Sorts the imports in alphabetical order
sortImports :: [Ann ImportDecl dom SrcTemplateStage] -> [Ann ImportDecl dom SrcTemplateStage]
sortImports = sortBy (compare `on` (^. element&importModule&element&AST.moduleNameString))

-- | Modify an import to only import  names that are used.
narrowImports :: forall n dom . OrganizeImportsDomain dom n 
              => [GHC.Name] -> [Ann ImportDecl dom SrcTemplateStage] -> LocalRefactor dom [Ann ImportDecl dom SrcTemplateStage]
narrowImports usedNames imps = foldM (narrowOneImport usedNames) imps imps 
  where narrowOneImport :: [GHC.Name] -> [Ann ImportDecl dom SrcTemplateStage] -> Ann ImportDecl dom SrcTemplateStage -> LocalRefactor dom [Ann ImportDecl dom SrcTemplateStage]
        narrowOneImport names all one =
          (\case Just x -> map (\e -> if e == one then x else e) all
                 Nothing -> delete one all) <$> narrowImport names (map (^. semantics) all) one 
        
narrowImport :: OrganizeImportsDomain dom n
             => [GHC.Name] -> [ImportInfo n] -> Ann ImportDecl dom SrcTemplateStage 
                           -> LocalRefactor dom (Maybe (Ann ImportDecl dom SrcTemplateStage))
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
narrowImportSpecs :: forall dom n . OrganizeImportsDomain dom n
                  => [GHC.Name] -> AnnList IESpec dom SrcTemplateStage -> LocalRefactor dom (AnnList IESpec dom SrcTemplateStage)
narrowImportSpecs usedNames 
  = (annList&element !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> IESpec dom SrcTemplateStage -> LocalRefactor dom (IESpec dom SrcTemplateStage)
        narrowSpecSubspec usedNames spec 
          = do let Just specName = semanticsName =<< (spec ^? ieName&element&simpleName&annotation&semanticInfo)
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> map getName (tyConDataCons tc) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: Ann IESpec dom SrcTemplateStage -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (semanticsName =<< (ie ^? element&ieName&element&simpleName&annotation&semanticInfo)) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? element&ieSubspec&annJust&element&essList&annList) /= [])
            || (case ie ^? element&ieSubspec&annJust&element of Just SubSpecAll -> True; _ -> False)     
  
narrowImportSubspecs :: OrganizeImportsDomain dom n => [GHC.Name] -> Ann SubSpec dom SrcTemplateStage -> Ann SubSpec dom SrcTemplateStage
narrowImportSubspecs [] (Ann _ SubSpecAll) = mkSubList []
narrowImportSubspecs _ ss@(Ann _ SubSpecAll) = ss
narrowImportSubspecs usedNames ss@(Ann _ (SubSpecList _)) 
  = element&essList .- filterList (\n -> fmap getName (semanticsName =<< (n ^? element&simpleName&annotation&semanticInfo)) `elem` map Just usedNames) $ ss
