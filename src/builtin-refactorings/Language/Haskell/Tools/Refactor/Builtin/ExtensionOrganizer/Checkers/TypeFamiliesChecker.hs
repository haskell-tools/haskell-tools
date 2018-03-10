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


-- | Checks whether any name's corresponding type in the module contains a type equality.
gblChkNamesForTypeEq :: CheckNode Module
gblChkNamesForTypeEq = conditionalAny gblChkNamesForTypeEq' [TypeFamilies, GADTs]

-- | Checks an operator for syntactic evidence of a ~ b type equality if TypeFamilies or GADTs is turned on.
chkOperatorForTypeEq :: CheckNode Operator
chkOperatorForTypeEq = conditionalAny chkOperatorForTypeEq' [TypeFamilies, GADTs]


-- | Checks a declaration if TypeFamilies is turned on.
chkTypeFamiliesDecl :: CheckNode Decl
chkTypeFamiliesDecl = conditional chkTypeFamiliesDecl' TypeFamilies

-- | Checks a class element if TypeFamilies is turned on.
chkTypeFamiliesClassElement :: CheckNode ClassElement
chkTypeFamiliesClassElement = conditional chkTypeFamiliesClassElement' TypeFamilies

-- | Checks an instance body if TypeFamilies is turned on.
chkTypeFamiliesInstBodyDecl :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl = conditional chkTypeFamiliesInstBodyDecl' TypeFamilies



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


chkOperatorForTypeEq' :: CheckNode Operator
chkOperatorForTypeEq' op
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addRelation (TypeFamilies `lOr` GADTs) op
  | otherwise = return op

chkNameForTyEqn :: CheckNode Name
chkNameForTyEqn name = do
  mty <- runMaybeT . lookupTypeFromId $ name
  case mty of
    Just ty -> do
      let ty'    = GHC.expandTypeSynonyms ty
          tycons = universeBi ty' :: [GHC.TyCon]
      if any isEqTyCon tycons then addRelation (TypeFamilies `lOr` GADTs) name
                              else return name
    Nothing -> return name

  --traceShow (showName name ++ " -- " ++ showOutputable ty') $

  where isEqTyCon tc = tc `hasKey` eqTyConKey
                    || tc `hasKey` heqTyConKey
                    || tc `hasKey` eqPrimTyConKey
                    || tc `hasKey` eqReprPrimTyConKey
                    || tc `hasKey` eqPhantPrimTyConKey

gblChkNamesForTypeEq' :: CheckNode Module
gblChkNamesForTypeEq' m = do
  let origNames   = universeBi (m ^. modDecl) :: [Name]
      pairedNames = catMaybes . zipWith zf (map semanticsName origNames) $ origNames
      uniqueNames = SMap.elems . SMap.fromList . reverse $ pairedNames
  mapM_ chkNameForTyEqn uniqueNames
  return m
  where zf (Just x) y = Just (x,y)
        zf _ _        = Nothing
