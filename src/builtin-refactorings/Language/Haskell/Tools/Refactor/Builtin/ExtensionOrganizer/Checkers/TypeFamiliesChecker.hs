module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeFamiliesChecker where

import TyCon          as GHC (TyCon())
import PrelNames      as GHC
import Unique         as GHC (hasKey)
import qualified Type as GHC (expandTypeSynonyms)

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.List (zipWith)
import Data.Maybe (catMaybes)
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as SMap

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup


-- | Checks whether any name's corresponding type in the module contains a type equality.
globalChkNamesForTypeEq :: CheckNode Module
globalChkNamesForTypeEq = conditional globalChkNamesForTypeEq' TypeFamilies

-- | Checks a declaration if TypeFamilies is turned on.
chkTypeFamiliesDecl :: CheckNode Decl
chkTypeFamiliesDecl = conditional chkTypeFamiliesDecl' TypeFamilies

-- | Checks a class element if TypeFamilies is turned on.
chkTypeFamiliesClassElement :: CheckNode ClassElement
chkTypeFamiliesClassElement = conditional chkTypeFamiliesClassElement' TypeFamilies

-- | Checks an instance body if TypeFamilies is turned on.
chkTypeFamiliesInstBodyDecl :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl = conditional chkTypeFamiliesInstBodyDecl' TypeFamilies

-- | Checks an assertion for syntactic evidence of a ~ b type equality if TypeFamilies is turned on.
chkTypeFamiliesAssertion :: CheckNode Assertion
chkTypeFamiliesAssertion = conditional chkTypeFamiliesAssertion' TypeFamilies

-- | Checks a type for syntactic evidence of a ~ b type equality if TypeFamilies is turned on.
chkTypeFamiliesType :: CheckNode Type
chkTypeFamiliesType = conditional chkTypeFamiliesType' TypeFamilies


chkTypeFamiliesDecl' :: CheckNode Decl
chkTypeFamiliesDecl' d@TypeFamily{}       = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@DataFamily{}       = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@ClosedTypeFamily{} = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@TypeInstance{}     = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@DataInstance{}     = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@GadtDataInstance{} = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d                    = return d

chkTypeFamiliesClassElement' :: CheckNode ClassElement
chkTypeFamiliesClassElement' ce@ClassElemTypeFam{} = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClassElemDataFam{} = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClsDefaultType{}   = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce                    = return ce

chkTypeFamiliesInstBodyDecl' :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl' b@InstanceTypeFamilyDef{}     = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyDef{}     = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyGADTDef{} = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b                             = return b

chkTypeFamiliesAssertion' :: CheckNode Assertion
chkTypeFamiliesAssertion' a@(InfixAssert _ op _)
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addOccurence TypeFamilies a
chkTypeFamiliesAssertion' a = return a

chkTypeFamiliesType' :: CheckNode Type
chkTypeFamiliesType' t@(InfixTypeApp _ op _)
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addOccurence TypeFamilies t
chkTypeFamiliesType' t = return t

chkNameForTyEqn :: CheckNode Name
chkNameForTyEqn name = do
  mty <- runMaybeT . lookupTypeFromId $ name
  case mty of
    Just ty -> do
      let ty'    = GHC.expandTypeSynonyms ty
          tycons = universeBi ty' :: [GHC.TyCon]
      if any isEqTyCon tycons then addOccurence TypeFamilies name
                              else return name
    Nothing -> return name

  --traceShow (showName name ++ " -- " ++ showOutputable ty') $

  where isEqTyCon tc = tc `hasKey` eqTyConKey
                    || tc `hasKey` heqTyConKey
                    || tc `hasKey` eqPrimTyConKey
                    || tc `hasKey` eqReprPrimTyConKey
                    || tc `hasKey` eqPhantPrimTyConKey

globalChkNamesForTypeEq' :: CheckNode Module
globalChkNamesForTypeEq' m = do
  let origNames   = universeBi (m ^. modDecl) :: [Name]
      pairedNames = catMaybes . zipWith zf (map semanticsName origNames) $ origNames
      uniqueNames = SMap.elems . SMap.fromList . reverse $ pairedNames
  mapM_ chkNameForTyEqn uniqueNames
  return m
  where zf (Just x) y = Just (x,y)
        zf _ _        = Nothing
