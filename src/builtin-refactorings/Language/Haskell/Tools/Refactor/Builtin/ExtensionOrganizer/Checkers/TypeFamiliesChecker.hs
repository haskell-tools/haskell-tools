module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeFamiliesChecker where

import TyCon          as GHC (TyCon())
import TyCoRep        as GHC (TyThing(..))
import PrelNames      as GHC (eqTyConName)
import Unique         as GHC (hasKey, getUnique)
import qualified Type as GHC (expandTypeSynonyms)

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup


-- | Checks a declaration if TypeFamilies is turned on.
chkTypeFamiliesDecl :: CheckNode Decl
chkTypeFamiliesDecl = conditional chkTypeFamiliesDecl' TypeFamilies

-- | Checks a class element if TypeFamilies is turned on.
chkTypeFamiliesClassElement :: CheckNode ClassElement
chkTypeFamiliesClassElement = conditional chkTypeFamiliesClassElement' TypeFamilies

-- | Checks an instance body if TypeFamilies is turned on.
chkTypeFamiliesInstBodyDecl :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl = conditional chkTypeFamiliesInstBodyDecl' TypeFamilies

-- | Checks an assertion (needed for a ~ b type equalities) if TypeFamilies is turned on.
chkTypeFamiliesAssertion :: CheckNode Assertion
chkTypeFamiliesAssertion = conditional chkTypeFamiliesAssertion' TypeFamilies

-- | Checks a type for infix type applications (needed for a ~ b type equalities) if TypeFamilies is turned on.
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
-- chkTypeFamiliesAssertion' a@(ClassAssert n _) = chkNameForTyEqn n >> return a
chkTypeFamiliesAssertion' a = return a

chkTypeFamiliesType' :: CheckNode Type
chkTypeFamiliesType' t@(InfixTypeApp _ op _)
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addOccurence TypeFamilies t
-- chkTypeFamiliesType' t@(VarType n) = chkNameForTyEqn n >> return t
chkTypeFamiliesType' t = return t

chkNameForTyEqn :: CheckNode Name
chkNameForTyEqn name = do
  mx <- runMaybeT . lookupTypeFromName $ name
  case mx of
    Nothing -> return name
    Just ty -> do
      let ty' = GHC.expandTypeSynonyms ty
          tycons = universeBi ty' :: [GHC.TyCon]
      if any isEqTyCon tycons then addOccurence TypeFamilies name
                              else return name
  where isEqTyCon tc = tc `hasKey` getUnique eqTyConName
