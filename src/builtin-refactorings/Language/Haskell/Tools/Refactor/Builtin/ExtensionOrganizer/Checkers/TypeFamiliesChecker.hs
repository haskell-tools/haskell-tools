module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeFamiliesChecker where

import TyCon          as GHC (TyCon())
import PrelNames      as GHC
import Unique         as GHC (hasKey)
import qualified Type as GHC (expandTypeSynonyms)

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.List (zipWith, (\\))
import Data.Maybe (catMaybes)
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as SMap

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad


-- | Checks whether any name's corresponding type in the module contains a type equality.
gblChkQNamesForTypeEq :: CheckNode Module
gblChkQNamesForTypeEq = conditionalAny gblChkQNamesForTypeEq' [TypeFamilies, GADTs]

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
chkTypeFamiliesDecl' d@TypeFamily{}       = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d@DataFamily{}       = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d@ClosedTypeFamily{} = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d@TypeInstance{}     = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d@DataInstance{}     = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d@GadtDataInstance{} = addEvidence TypeFamilies d
chkTypeFamiliesDecl' d                    = return d

chkTypeFamiliesClassElement' :: CheckNode ClassElement
chkTypeFamiliesClassElement' ce@ClassElemTypeFam{} = addEvidence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClassElemDataFam{} = addEvidence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClsDefaultType{}   = addEvidence TypeFamilies ce
chkTypeFamiliesClassElement' ce                    = return ce

chkTypeFamiliesInstBodyDecl' :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl' b@InstanceTypeFamilyDef{}     = addEvidence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyDef{}     = addEvidence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyGADTDef{} = addEvidence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b                             = return b


chkOperatorForTypeEq' :: CheckNode Operator
chkOperatorForTypeEq' op
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addRelation (TypeFamilies `lOr` GADTs) op
  | otherwise = return op

chkQNameForTyEqnWith :: (LogicalRelation Extension -> CheckNode QualifiedName) ->
                         CheckNode QualifiedName
chkQNameForTyEqnWith addRel name = do
  mty <- runMaybeT . lookupTypeFromId $ name
  case mty of
    Just ty -> do
      let ty'    = GHC.expandTypeSynonyms ty
          tycons = universeBi ty' :: [GHC.TyCon]
      if any isEqTyCon tycons then addRel (TypeFamilies `lOr` GADTs) name
                              else return name
    Nothing -> return name

  --traceShow (showName name ++ " -- " ++ showOutputable ty') $

  where isEqTyCon tc = tc `hasKey` eqTyConKey
                    || tc `hasKey` heqTyConKey
                    || tc `hasKey` eqPrimTyConKey
                    || tc `hasKey` eqReprPrimTyConKey
                    || tc `hasKey` eqPhantPrimTyConKey

gblChkQNamesForTypeEq' :: CheckNode Module
gblChkQNamesForTypeEq' m = do
  -- names appearing on the rhs of bindings are just hints
  -- only lhs names require TypeFamilies
  let origNames   = universeBi (m ^. modDecl) :: [QualifiedName]
      rhs         = universeBi (m ^. modDecl) :: [Rhs]
      hints       = universeBi rhs            :: [QualifiedName]
      evidence    = origNames \\ hints
      pairedNames = catMaybes . zipWith zf (map semanticsName evidence) $ evidence
      uniqueNames = SMap.elems . SMap.fromList . reverse $ pairedNames
  mapM_ (chkQNameForTyEqnWith addRelation)     uniqueNames
  mapM_ (chkQNameForTyEqnWith addRelationHint) hints
  return m
  where zf (Just x) y = Just (x,y)
        zf _ _        = Nothing
