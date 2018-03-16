module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleContextsChecker where

import CoAxiom (Role(..))
import Kind (returnsConstraintKind)
import TcType (tcSplitNestedSigmaTys, checkValidClsArgs)
import Type hiding (Type(..))
import PrelNames (eqTyConName, eqTyConKey)
import Unique (hasKey)
import qualified Name as GHC
import qualified GHC

import Data.List
import Data.Function (on)
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as SMap

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

instance Eq GHC.Type where
  (==) = eqType

gblChkQNamesForFC :: CheckNode Module
gblChkQNamesForFC = conditional gblChkQNamesForFC' FlexibleContexts

chkFlexibleContexts :: CheckNode Context
chkFlexibleContexts = conditional chkFlexibleContexts' FlexibleContexts

chkFlexibleContexts' :: CheckNode Context
chkFlexibleContexts' ctx@(Context a) = chkAssertion a >> return ctx

chkAssertion :: CheckNode Assertion
chkAssertion a@(ClassAssert con annTys) = do
  -- If a lookup fails, we presume that it is a class
  isClass <- fromMaybeT True (isClassTyConNameM con)
  if not isClass || all hasTyVarHead (annTys ^. annListElems)
    then return a
    else addEvidence FlexibleContexts a
chkAssertion a@(InfixAssert lhs op rhs)
  -- If the constraint is of form a ~ b, then we dont need to check
  | Just name <- semanticsName op
  , name == eqTyConName
  = return a
  -- If the constraint has only type variable head, it doesn't need FlexibleContexts
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
      let expanded = expandTypeSynonyms ty
          allTys = universeBi expanded :: [GHC.Type]
          (preds, thetas) = partition isPredTy . filter (not . isIPPred) $ allTys
          nomEqs = universeBi (filter isNominalEqPred preds) :: [GHC.Type]
          preds' = preds \\ nomEqs
          thetas' = concatMap (snd' . tcSplitNestedSigmaTys) thetas
          xs = mapMaybe getClassPredTys_maybe (thetas' ++ preds')
          xs' = filter (not . isCTupleClass . fst) xs
      if all (uncurry hasValidClsArgs) xs'
        then return False
        else return True
    Nothing -> return False
  where hasValidClsArgs cls args = res
          where res = cls `hasKey` eqTyConKey
                   || isIPClass cls
                   || checkValidClsArgs False cls args
        snd' (_,x,_) = x
        isNominalEqPred ty
          | Just (tc,_) <- splitTyConApp_maybe ty = tc `hasKey` eqTyConKey
          | otherwise = False

gblChkQNamesForFC' :: CheckNode Module
gblChkQNamesForFC' m = do
  -- names appearing on the rhs of bindings are just hints
  -- only lhs names require FlexibleContexts
  let origNames   = universeBi (m ^. modDecl) :: [QualifiedName]
      rhs         = universeBi (m ^. modDecl) :: [Rhs]
      hints       = universeBi rhs            :: [QualifiedName]

      -- selecting relevant names
      -- and grouping them together based on their GHC.Name
      evidence    = origNames \\ hints
      pairedNames = catMaybes . zipWith zf (map semanticsName evidence) $ evidence
      groupedNames = map (map snd)
                   . groupBy ((==) `on` fst)
                   . sortBy (compare `on` fst)
                   $ pairedNames

      -- selecting a representative element from the names
      representatives = map ((,) <$> head <*> id) groupedNames

  es <- filterM (chkQNameForFC . fst) representatives
  hs <- filterM chkQNameForFC hints
  mapM_ (addEvidence FlexibleContexts) (concatMap snd es)
  mapM_ (addHint     FlexibleContexts) hs
  return m
  where zf (Just x) y = Just (x,y)
        zf _ _        = Nothing
