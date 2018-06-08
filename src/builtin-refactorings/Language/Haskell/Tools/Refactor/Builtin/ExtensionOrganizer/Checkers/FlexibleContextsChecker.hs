module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleContextsChecker where

import Kind (returnsConstraintKind)
import TcType (tcSplitNestedSigmaTys, checkValidClsArgs)
import Type hiding (Type(..))
import PrelNames (eqTyConName, eqTyConKey)
import Unique (hasKey)
import qualified Name as GHC
import qualified GHC

import Data.List
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

gblChkQNamesForFC :: CheckNode Module
gblChkQNamesForFC = conditional gblChkQNamesForFC' FlexibleContexts

chkFlexibleContexts :: CheckNode Context
chkFlexibleContexts = conditional chkFlexibleContexts' FlexibleContexts

chkFlexibleContexts' :: CheckNode Context
chkFlexibleContexts' ctx@(Context a) = chkAssertion a >> return ctx

chkAssertion :: CheckNode Assertion
chkAssertion a@(ClassAssert con annTys) = do
  -- If a lookup fails, add MissingInformation FlexibleContexts
  let errDefault = addMI FlexibleContexts a >> return False
  isClass <- fromMaybeTM errDefault (isClassTyConNameM con)
  if not isClass || all hasTyVarHead (annTys ^. annListElems)
    then return a
    else addEvidence FlexibleContexts a
chkAssertion a@(InfixAssert lhs op rhs)
  -- If the constraint is of form a ~ b, then we dont need to check
  | Just name <- semanticsName op
  , name == eqTyConName
  = return a
  -- If the constraint has only type variable heads, it doesn't need FlexibleContexts
  | hasTyVarHead lhs && hasTyVarHead rhs = return a
  -- In any other case, we keep FlexibleContexts
  | otherwise = addEvidence FlexibleContexts a
chkAssertion a@(TupleAssert xs) = do
  mapM_ chkAssertion xs
  return a
chkAssertion a@ImplicitAssert{} = return a

chkFlexibleContextsDecl :: CheckNode Decl
chkFlexibleContextsDecl = conditional chkFlexibleContextsDecl' FlexibleContexts

chkFlexibleContextsDecl' :: CheckNode Decl
chkFlexibleContextsDecl' d@(TypeDecl dh rhs) = do
  let ty = typeOrKindFromId . declHeadQName $ dh
  when (hasConstraintKind ty || returnsConstraintKind ty)
       (chkClassesInside rhs)
  return d
chkFlexibleContextsDecl' d = return d

-- | Checks whether all type class applications in a type synonym rhs
-- (that has kind Constraint) have only type variable heads.
-- Returns False if a lookup inside is not succesful.
-- If it isn't applied to a type that has kind Constraint, it may give
-- false positive results.
chkClassesInside :: Type -> ExtMonad ()
chkClassesInside (TupleType annTys) =
  mapM_ chkClassesInside (annTys ^. annListElems)
chkClassesInside t = maybe (addFC t) f (splitTypeAppMaybe t)
  where
    addFC :: Type -> ExtMonad ()
    addFC = addEvidence_ FlexibleContexts

    f :: (GHC.Name, [Type]) -> ExtMonad ()
    f (n,args) = do
       isClass <- isJustT . lookupClass $ n
       when (n /= eqTyConName && isClass && any (not . hasTyVarHead) args)
            (addFC t)

    -- Separates the type into its head and its arguments.
    -- This functions should only be applied to types that have kind Constraint.
    splitTypeAppMaybe :: Type -> Maybe (GHC.Name, [Type])
    splitTypeAppMaybe (TypeApp f arg) = do
      (f', args) <- splitTypeAppMaybe f
      return (f', args ++ [arg])
    splitTypeAppMaybe (InfixTypeApp lhs op rhs) = do
      opName <- semanticsName op
      return (opName, [lhs,rhs])
    splitTypeAppMaybe (VarType n) = do
      sname <- semanticsName n
      return (sname, [])
    splitTypeAppMaybe (ParenType t) = splitTypeAppMaybe t
    splitTypeAppMaybe (KindedType t _) = splitTypeAppMaybe t
    splitTypeAppMaybe _ = Nothing


chkQNameForFC :: QualifiedName -> ExtMonad Bool
chkQNameForFC name = do
  mty <- runMaybeT . lookupTypeFromId $ name
  case mty of
    Just ty -> do
      -- First we get all subtypes of the given type, while expanding all type synonyms,
      -- then we remove all implicit predicates,
      -- and partition the types into predicate types and theta types.
      let expanded = expandTypeSynonyms ty
          subTys = universeBi expanded :: [GHC.Type]
          (preds, tys) = partition isPredTy . filter (not . isIPPred) $ subTys

          -- We don't need type equalities, so we get rid of them and their children.
          -- Then we extract the constraints from the types.
          nomEqs = universeBi (filter isNominalEqPred preds) :: [GHC.Type]
          preds' = preds \\ nomEqs
          thetas = concatMap (snd' . tcSplitNestedSigmaTys) tys

          -- We split each theta type into its class tycon and its arguments.
          -- We also filter out tuple constraints
          -- (we already have the constraints in it).
          xs = mapMaybe getClassPredTys_maybe (thetas ++ preds')
          xs' = filter (not . isCTupleClass . fst) xs

      if all (uncurry hasValidClsArgs) xs' then return False
                                           else return True
    Nothing -> return False
  where
    hasValidClsArgs :: GHC.Class -> [GHC.Type] -> Bool
    hasValidClsArgs cls args = cls `hasKey` eqTyConKey
                            || isIPClass cls
                            || checkValidClsArgs False cls args

    snd' :: (a,b,c) -> b
    snd' (_,x,_) = x

    isNominalEqPred :: GHC.Type -> Bool
    isNominalEqPred ty
      | Just (tc,_) <- splitTyConApp_maybe ty = tc `hasKey` eqTyConKey
      | otherwise = False

gblChkQNamesForFC' :: CheckNode Module
gblChkQNamesForFC' m = do
  -- Names appearing on the rhs of bindings are just hints,
  -- they don't necessarily require FlexibleContexts.
  let allNames   = universeBi (m ^. modDecl) :: [QualifiedName]
      rhs        = universeBi (m ^. modDecl) :: [Rhs]
      hints      = universeBi rhs            :: [QualifiedName]

      -- Separating hints from evidence,
      -- and grouping them together based on their GHC.Name
      evidence  = filter (isJust . semanticsName) (allNames \\ hints)
      hints'    = filter (isJust . semanticsName) hints
      groupedEs = equivalenceGroupsBy semanticsName evidence
      groupedHs = equivalenceGroupsBy semanticsName hints'

  -- Checking the representative elements, whether they need FC,
  -- if they do, add occurences for every node in their group.
  es <- filterM (chkQNameForFC . fst) groupedEs
  hs <- filterM (chkQNameForFC . fst) groupedHs
  mapM_ (addEvidence FlexibleContexts) (concatMap snd es)
  mapM_ (addHint     FlexibleContexts) (concatMap snd hs)

  return m
