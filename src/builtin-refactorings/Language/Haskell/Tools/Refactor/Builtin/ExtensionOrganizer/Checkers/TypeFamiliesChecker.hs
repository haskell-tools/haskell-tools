module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeFamiliesChecker where

import TyCon          as GHC (TyCon())
import PrelNames      as GHC
import Unique         as GHC (hasKey)
import Var            as GHC (isId)
import qualified Type as GHC (expandTypeSynonyms)

import Control.Reference ((^.))

import Data.List ((\\))
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations

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

-- | Checks whether a given name's has a type equality operator in it.
-- If the type lookup fails, it returns Nothing. If given a type variable,
-- it returns False.
chkQNameForTyEqn :: QualifiedName -> MaybeT ExtMonad Bool
chkQNameForTyEqn name =
  if not . isId . semanticsId $ name
    then return False
    else do
      ty <- lookupTypeFromId name
      let ty'    = GHC.expandTypeSynonyms ty
          tycons = universeBi ty' :: [GHC.TyCon]
      if any isEqTyCon tycons then return True
                              else return False
      where isEqTyCon tc = tc `hasKey` eqTyConKey
                        || tc `hasKey` heqTyConKey
                        || tc `hasKey` eqPrimTyConKey
                        || tc `hasKey` eqReprPrimTyConKey
                        || tc `hasKey` eqPhantPrimTyConKey

gblChkQNamesForTypeEq' :: CheckNode Module
gblChkQNamesForTypeEq' m = do
  -- Names appearing on the rhs of bindings are just hints,
  -- they don't necessarily require TypeFamilies.
  let allNames   = universeBi (m ^. modDecl) :: [QualifiedName]
      rhs        = universeBi (m ^. modDecl) :: [Rhs]
      hints      = universeBi rhs            :: [QualifiedName]

      -- Separating hints from evidence,
      -- and grouping them together based on their GHC.Name
      evidence  = filter (isJust . semanticsName) (allNames \\ hints)
      hints'    = filter (isJust . semanticsName) hints
      groupedEs = equivalenceGroupsBy semanticsName evidence
      groupedHs = equivalenceGroupsBy semanticsName hints'

      -- names failed at the semanticsName lookup
      nmFailedNames = filter (isNothing . semanticsName) allNames

  -- Checking the representative elements, whether they need FC,
  -- if they do, add occurences for every node in their group.
  -- If chkQNameForTyEqn fails, we add MissingInformation.
  let hasTypeEq    = fromMaybeT False . chkQNameForTyEqn

  es <- filterM (hasTypeEq . fst) groupedEs
  hs <- filterM (hasTypeEq . fst) groupedHs
  mapM_ (addRelation     (TypeFamilies `lOr` GADTs)) (concatMap snd es)
  mapM_ (addRelationHint (TypeFamilies `lOr` GADTs)) (concatMap snd hs)

  mapM_ (addRelationMI (TypeFamilies `lOr` GADTs)) nmFailedNames

  -- NOTE: this would result in many false positive results
  -- names failed at the chkQNameForTyEqn (lookupTypeFromId) stage
  -- idFailedNames <- filterM failedLookup allNames
  -- mapM_ (addRelationMI (TypeFamilies `lOr` GADTs)) idFailedNames

  return m
