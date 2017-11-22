
-- | The transformation monad carries the necessary information that is passed top-down
-- during the conversion from GHC AST to our representation.
module Language.Haskell.Tools.BackendGHC.Monad where

import Control.Applicative ((<|>))
import Control.Exception (Exception, evaluate, throw)
import Control.Monad.Reader
import Control.Reference
import Data.Function (on)
import Data.List
import Data.Map as Map (Map, lookup, empty)
import Data.Maybe
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.SemaInfoTypes (PName(..), UsageSpec(..))
import Language.Haskell.Tools.BackendGHC.GHCUtils (HsHasName(..), rdrNameStr)
import Language.Haskell.Tools.BackendGHC.SourceMap (SourceMap, annotationsToSrcMap)

import ApiAnnotation (ApiAnnKey)
import DynFlags (xopt_set)
import ErrUtils (pprErrMsgBagWithLoc)
import GHC
import GHC.LanguageExtensions.Type (Extension(..))
import HscTypes (HscEnv(..))
import Name (Name, isVarName, isTyVarName)
import OccName as GHC (HasOccName(..), mkOccEnv)
import Outputable hiding (empty)
import RdrName
import RnEnv (mkUnboundNameRdr)
import RnExpr (rnLExpr)
import TcRnMonad
import TcRnTypes (TcGblEnv(..))

-- | The transformation monad type
type Trf = ReaderT TrfInput Ghc

-- | The (immutable) data for the transformation
data TrfInput
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , pragmaComms :: Map String [Located String] -- ^ Pragma comments
             , declsToInsert :: [Ann UDecl (Dom RdrName) RangeStage] -- ^ Declarations that are from the parsed AST
             , contRange :: SrcSpan -- ^ The focus of the transformation
             , localsInScope :: [[(GHC.Name, Maybe [UsageSpec], Maybe Name)]] -- ^ Local names visible
             , defining :: Bool -- ^ True, if names are defined in the transformed AST element.
             , definingTypeVars :: Bool -- ^ True, if type variable names are defined in the transformed AST element.
             , originalNames :: Map SrcSpan RdrName -- ^ Stores the original format of names.
             , declSplices :: [Located (HsSplice GHC.RdrName)] -- ^ Location of the TH splices for extracting declarations from the renamed AST.
                 -- ^ It is possible that multiple declarations stand in the place of the declaration splice or none at all.
             , typeSplices :: [Located (HsSplice GHC.RdrName)] -- ^ Type splices
             , exprSplices :: [Located (HsSplice GHC.RdrName)] -- ^ Expression splices
             }

trfInit :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> TrfInput
trfInit annots comments
  = TrfInput { srcMap = annotationsToSrcMap annots
             , pragmaComms = comments
             , declsToInsert = []
             , contRange = noSrcSpan
             , localsInScope = []
             , defining = False
             , definingTypeVars = False
             , originalNames = empty
             , declSplices = []
             , typeSplices = []
             , exprSplices = []
             }

liftGhc :: Ghc a -> Trf a
liftGhc = lift

-- | Perform the transformation taking names as defined.
define :: Trf a -> Trf a
define = local (\s -> s { defining = True })

-- | Perform the transformation taking type variable names as defined.
defineTypeVars :: Trf a -> Trf a
defineTypeVars = local (\s -> s { definingTypeVars = True })

-- | Transform as type variables
typeVarTransform :: Trf a -> Trf a
typeVarTransform = local (\s -> s { defining = defining s || definingTypeVars s })

-- | Transform a name as a type variable if it is one.
transformingPossibleVar :: HsHasName n => n -> Trf a -> Trf a
transformingPossibleVar n = case hsGetNames Nothing n of
  [(name,_)] | isVarName name || isTyVarName name -> typeVarTransform
  _                                               -> id

-- | Perform the transformation putting the given definition in a new local scope.
addEmptyScope :: Trf a -> Trf a
addEmptyScope = local (\s -> s { localsInScope = [] : localsInScope s })

-- | Perform the transformation putting the given definition in a new local scope.
addToScopeImported :: [(String, Maybe String, Bool, [PName GHC.Name])] -> Trf a -> Trf a
addToScopeImported ls = local (\s -> s { localsInScope = concatMap (\(mn, asName, q, e) -> map (\(PName n p) -> (n, Just [UsageSpec q mn (fromMaybe mn asName)], p)) e) ls : localsInScope s })


-- | Perform the transformation putting the given definition in a new local scope.
addToScope :: HsHasName e => e -> Trf a -> Trf a
addToScope e = local (\s -> s { localsInScope = map (\(n,p) -> (n, Nothing, p)) (hsGetNames Nothing e) : localsInScope s })

-- | Perform the transformation putting the given definitions in the current scope.
addToCurrentScope :: HsHasName e => e -> Trf a -> Trf a
addToCurrentScope e = local (\s -> s { localsInScope = case localsInScope s of lastScope:rest -> (map (\(n,p) -> (n, Nothing, p)) (hsGetNames Nothing e) ++ lastScope):rest
                                                                               []             -> [map (\(n,p) -> (n, Nothing, p)) (hsGetNames Nothing e)] })

-- | Performs the transformation given the tokens of the source file
runTrf :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> Trf a -> Ghc a
runTrf annots comments trf = runReaderT trf (trfInit annots comments)

setOriginalNames :: Map SrcSpan RdrName -> Trf a -> Trf a
setOriginalNames names = local (\s -> s { originalNames = names })

-- | Get the original format of a name (before scoping).
getOriginalName :: RdrName -> Trf String
getOriginalName n = do sp <- asks contRange
                       asks (rdrNameStr . fromMaybe n . (Map.lookup sp) . originalNames)

-- | Set splices that must replace the elements that are generated into the AST representation.
setSplices :: [Located (HsSplice GHC.RdrName)] -> [Located (HsSplice GHC.RdrName)] -> [Located (HsSplice GHC.RdrName)] -> Trf a -> Trf a
setSplices declSpls typeSpls exprSpls
  = local (\s -> s { typeSplices = typeSpls, exprSplices = exprSpls, declSplices = declSpls })

-- | Set the list of declarations that will be missing from AST
setDeclsToInsert :: [Ann UDecl (Dom RdrName) RangeStage] -> Trf a -> Trf a
setDeclsToInsert decls = local (\s -> s {declsToInsert = decls})

-- Remove the splice that has already been added
exprSpliceInserted :: Located (HsSplice n) -> Trf a -> Trf a
exprSpliceInserted spl = local (\s -> s { exprSplices = Prelude.filter (\sp -> getLoc sp /= getLoc spl) (exprSplices s) })

-- Remove the splice that has already been added
typeSpliceInserted :: Located (HsSplice n) -> Trf a -> Trf a
typeSpliceInserted spl = local (\s -> s { typeSplices = Prelude.filter (\sp -> getLoc sp /= getLoc spl) (typeSplices s) })

rdrSplice :: HsSplice RdrName -> Trf (HsSplice GHC.Name)
rdrSplice spl = do
    rng <- asks contRange
    env <- liftGhc getSession
    locals <- unifyScopes [] <$> asks localsInScope
    let createLocalGRE (n,imp,p) = [GRE n (maybe NoParent ParentIs p) (isNothing imp) (maybe [] (map createGREImport) imp) ]
        createGREImport (UsageSpec q useQ asQ) = ImpSpec (ImpDeclSpec (mkModuleName useQ) (mkModuleName asQ) q noSrcSpan) ImpAll
    let readEnv = mkOccEnv $ map (foldl1 (\e1 e2 -> (fst e1, snd e1 ++ snd e2))) $ groupBy ((==) `on` fst) $ sortOn fst
                   $ map (\n -> (GHC.occName ((^. _1) n), createLocalGRE n))
                   -- group up locals by name
                   $ map (foldl1 (\e1 e2 -> ((^. _1) e1, (^. _2) e1 `mappend` (^. _2) e2, (^. _3) e1 <|> (^. _3) e2)))
                   $ groupBy ((==) `on` (^. _1)) $ sortBy (compare `on` (^. _1)) locals
    tcSpl <- liftIO $ runTcInteractive env { hsc_dflags = xopt_set (hsc_dflags env) TemplateHaskellQuotes }
      $ updGblEnv (\gbl -> gbl { tcg_rdr_env = readEnv })
      $ tcHsSplice' spl
    let typecheckErrors = showSDocUnsafe (vcat (pprErrMsgBagWithLoc (fst (fst tcSpl)))
                                            <+> vcat (pprErrMsgBagWithLoc (snd (fst tcSpl))))
    -- This function refers the ghc environment, we must evaluate the result or the reference
    -- may be kept preventing garbage collection.
    liftIO $ evaluate (snd tcSpl)
    return $ fromMaybe (throw $ SpliceInsertionProblem rng typecheckErrors)
                       (snd tcSpl)
  where
    tcHsSplice' (HsTypedSplice dec id e)
      = HsTypedSplice dec (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsUntypedSplice dec id e)
      = HsUntypedSplice dec (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsQuasiQuote id1 id2 sp fs)
      = pure $ HsQuasiQuote (mkUnboundNameRdr id1) (mkUnboundNameRdr id2) sp fs


    unifyScopes :: [GHC.Name] -> [[(GHC.Name, Maybe [UsageSpec], Maybe GHC.Name)]] -> [(GHC.Name, Maybe [UsageSpec], Maybe GHC.Name)]
    unifyScopes _ [] = []
    unifyScopes ex (sc:scs) = filteredSc ++ unifyScopes (ex ++ map (^. _1) filteredSc) scs
      where filteredSc = filter ((\s -> isNothing $ find (\e -> occName e == occName s) ex) . (^. _1)) sc

data SpliceInsertionProblem = SpliceInsertionProblem SrcSpan String
  deriving Show

instance Exception SpliceInsertionProblem
