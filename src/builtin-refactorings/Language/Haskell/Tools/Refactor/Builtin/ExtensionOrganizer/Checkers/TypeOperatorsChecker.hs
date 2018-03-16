module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeOperatorsChecker where

import PrelNames (eqTyConName)
import qualified Name    as GHC (nameOccName)
import qualified OccName as GHC (isTcOcc, isSymOcc)

import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations

import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- | Checks a Type whether it contains any DeclHeads that amy need TypeOperators
chkTypeOperatorsType :: CheckNode Type
chkTypeOperatorsType = conditional chkTypeOperatorsType' TypeOperators

-- | Checks an Assertion whether it contains any DeclHeads that amy need TypeOperators
chkTypeOperatorsAssertion :: CheckNode Assertion
chkTypeOperatorsAssertion = conditional chkTypeOperatorsAssertion' TypeOperators

-- | Checks an InstanceHEad whether it contains any DeclHeads that amy need TypeOperators
chkTypeOperatorsInstHead :: CheckNode InstanceHead
chkTypeOperatorsInstHead = conditional chkTypeOperatorsInstHead' TypeOperators

-- | Checks a Decl whether it contains any DeclHeads that amy need TypeOperators
-- We check Decls to avoid getting multiple occurences of the same DeclHead
-- (e.g. we would check it with and without parentheses)
chkTypeOperatorsDecl :: CheckNode Decl
chkTypeOperatorsDecl = conditional chkTypeOperatorsDecl' TypeOperators



chkTypeOperatorsType' :: CheckNode Type
chkTypeOperatorsType' t@(InfixTypeApp _ op _)
  | Just name <- semanticsName op
  , name == eqTyConName
  = return t
  | otherwise = addEvidence TypeOperators t
chkTypeOperatorsType' t = return t

chkTypeOperatorsAssertion' :: CheckNode Assertion
chkTypeOperatorsAssertion' a@InfixAssert{} = addEvidence TypeOperators a
chkTypeOperatorsAssertion' a = return a

chkTypeOperatorsInstHead' :: CheckNode InstanceHead
chkTypeOperatorsInstHead' ih@InfixInstanceHead{} = addEvidence TypeOperators ih
chkTypeOperatorsInstHead' ih = return ih

chkTypeOperatorsDecl' :: CheckNode Decl
chkTypeOperatorsDecl' d = do
  let dhs = universeBi d :: [DeclHead]
  anyNeedsTO <- liftM or $ mapM isOperatorM dhs
  if anyNeedsTO then addEvidence TypeOperators d
                else return d

-- OccName: [Type and class operator definitions]
  -- We are not just looking for a *syntactically-infix* declaration,
  -- but one that uses an operator OccName.
isOperatorM :: DeclHead -> ExtMonad Bool
isOperatorM dh = do
  let mSemName = declHeadSemName dh
  case mSemName of
    Just semName
      | occ <- GHC.nameOccName semName
      , GHC.isTcOcc occ && GHC.isSymOcc occ
      -> return True
      | otherwise -> return False
    Nothing -> return True
