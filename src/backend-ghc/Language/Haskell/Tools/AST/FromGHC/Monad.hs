{-# LANGUAGE TupleSections #-}
-- | The transformation monad carries the necessary information that is passed top-down
-- during the conversion from GHC AST to our representation.
module Language.Haskell.Tools.AST.FromGHC.Monad where

import Control.Monad.Reader
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Map as Map (Map, lookup, empty)
import Data.Maybe (fromMaybe)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC.GHCUtils (HsHasName(..), rdrNameStr)
import Language.Haskell.Tools.AST.FromGHC.SourceMap (SourceMap, annotationsToSrcMap)

import ApiAnnotation (ApiAnnKey)
import GHC
import Name (Name, isVarName, isTyVarName)
import HscTypes
import SrcLoc
import TcRnTypes
import OccName as GHC
import RdrName
import RnEnv
import DynFlags
import RnExpr
import ErrUtils
import Outputable hiding (empty)
import TcRnMonad
import GHC.LanguageExtensions.Type

-- | The transformation monad type
type Trf = ReaderT TrfInput Ghc

-- | The (immutable) data for the transformation
data TrfInput
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , pragmaComms :: Map String [Located String] -- ^ Pragma comments
             , declsToInsert :: [Ann UDecl (Dom RdrName) RangeStage] -- ^ Declarations that are from the parsed AST
             , contRange :: SrcSpan -- ^ The focus of the transformation
             , localsInScope :: [[(GHC.Name, Maybe [UsageSpec])]] -- ^ Local names visible
             , defining :: Bool -- ^ True, if names are defined in the transformed AST element.
             , definingTypeVars :: Bool -- ^ True, if type variable names are defined in the transformed AST element.
             , originalNames :: Map SrcSpan RdrName -- ^ Stores the original format of names.
             , declSplices :: [Located (HsSplice GHC.RdrName)] -- ^ Location of the TH splices for extracting declarations from the renamed AST.
                 -- ^ It is possible that multiple declarations stand in the place of the declaration splice or none at all.
             , typeSplices :: [HsSplice GHC.RdrName] -- ^ Other types of splices (expressions, types).
             , exprSplices :: [HsSplice GHC.RdrName] -- ^ Other types of splices (expressions, types).
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
transformingPossibleVar n = case hsGetNames n of
  [name] | isVarName name || isTyVarName name -> typeVarTransform
  _                                           -> id

-- | Perform the transformation putting the given definition in a new local scope.
addEmptyScope :: Trf a -> Trf a
addEmptyScope = local (\s -> s { localsInScope = [] : localsInScope s })

-- | Perform the transformation putting the given definition in a new local scope.
addToScopeImported :: [(String, Maybe String, Bool, [GHC.Name])] -> Trf a -> Trf a
addToScopeImported ls = local (\s -> s { localsInScope = concatMap (\(mn, asName, q, e) -> map (, Just [UsageSpec q mn (fromMaybe mn asName)]) e) ls : localsInScope s })


-- | Perform the transformation putting the given definition in a new local scope.
addToScope :: HsHasName e => e -> Trf a -> Trf a
addToScope e = local (\s -> s { localsInScope = map (, Nothing) (hsGetNames e) : localsInScope s })

-- | Perform the transformation putting the given definitions in the current scope.
addToCurrentScope :: HsHasName e => e -> Trf a -> Trf a
addToCurrentScope e = local (\s -> s { localsInScope = case localsInScope s of lastScope:rest -> (map (, Nothing) (hsGetNames e) ++ lastScope):rest
                                                                               []             -> [map (, Nothing) (hsGetNames e)] })

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
setSplices :: [Located (HsSplice GHC.RdrName)] -> [HsSplice GHC.RdrName] -> [HsSplice GHC.RdrName] -> Trf a -> Trf a
setSplices declSpls typeSpls exprSpls
  = local (\s -> s { typeSplices = typeSpls, exprSplices = exprSpls, declSplices = declSpls })

-- | Set the list of declarations that will be missing from AST
setDeclsToInsert :: [Ann UDecl (Dom RdrName) RangeStage] -> Trf a -> Trf a
setDeclsToInsert decls = local (\s -> s {declsToInsert = decls})

-- Remove the splice that has already been added
exprSpliceInserted :: HsSplice n -> Trf a -> Trf a
exprSpliceInserted spl = local (\s -> s { exprSplices = Prelude.filter (\sp -> getSpliceLoc sp /= spLoc) (exprSplices s) })
  where spLoc = getSpliceLoc spl

-- Remove the splice that has already been added
typeSpliceInserted :: HsSplice n -> Trf a -> Trf a
typeSpliceInserted spl = local (\s -> s { typeSplices = Prelude.filter (\sp -> getSpliceLoc sp /= spLoc) (typeSplices s) })
  where spLoc = getSpliceLoc spl


getSpliceLoc :: HsSplice a -> SrcSpan
getSpliceLoc (HsTypedSplice _ e) = getLoc e
getSpliceLoc (HsUntypedSplice _ e) = getLoc e
getSpliceLoc (HsQuasiQuote _ _ sp _) = sp
getSpliceLoc (HsSpliced _ _) = noSrcSpan

rdrSplice :: HsSplice RdrName -> Trf (HsSplice GHC.Name)
rdrSplice spl = do
    env <- liftGhc getSession
    locals <- concat <$> asks localsInScope
    let createLocalGRE (n,imp) = [GRE n NoParent (isNothing imp) (maybe [] (map createGREImport) imp) ]
        createGREImport (UsageSpec q useQ asQ) = ImpSpec (ImpDeclSpec (mkModuleName useQ) (mkModuleName asQ) q noSrcSpan) ImpAll
        readEnv = mkOccEnv $ map (foldl1 (\e1 e2 -> (fst e1, snd e1 ++ snd e2)) . map snd) $ groupBy ((==) `on` fst) $ sortOn fst
                   $ (map (\n -> (fst n, (GHC.occName (fst n), createLocalGRE n))) locals)
    tcSpl <- liftIO $ runTcInteractive env { hsc_dflags = xopt_set (hsc_dflags env) TemplateHaskellQuotes }
      $ updGblEnv (\gbl -> gbl { tcg_rdr_env = readEnv })
      $ tcHsSplice' spl
    return $ fromMaybe (error $ "Splice expression could not be typechecked: "
                                   ++ showSDocUnsafe (vcat (pprErrMsgBagWithLoc (fst (fst tcSpl)))
                                                       <+> vcat (pprErrMsgBagWithLoc (snd (fst tcSpl)))))
                       (snd tcSpl)
  where
    tcHsSplice' (HsTypedSplice id e)
      = HsTypedSplice (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsUntypedSplice id e)
      = HsUntypedSplice (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsQuasiQuote id1 id2 sp fs)
      = pure $ HsQuasiQuote (mkUnboundNameRdr id1) (mkUnboundNameRdr id2) sp fs
