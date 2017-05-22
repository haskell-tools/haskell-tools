{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
-- | Functions that convert the value and function definitions of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Binds where

import ApiAnnotation as GHC (AnnKeywordId(..))
import Bag as GHC (bagToList)
import BasicTypes as GHC (FixityDirection(..), Fixity(..))
import BasicTypes as GHC
import HsBinds as GHC
import HsExpr as GHC
import HsPat as GHC (LPat)
import HsTypes as GHC (HsWildCardBndrs(..), HsImplicitBndrs(..))
import Name as GHC (isSymOcc)
import SrcLoc as GHC

import Control.Monad.Reader (Monad(..), mapM, asks)
import Data.List

import Language.Haskell.Tools.AST.FromGHC.Exprs (trfExpr)
import Language.Haskell.Tools.AST.FromGHC.GHCUtils (occName)
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Patterns (trfPattern)
import Language.Haskell.Tools.AST.FromGHC.Types (trfType)
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfBind :: TransformName n r => Located (HsBind n) -> Trf (Ann AST.UValueBind (Dom r) RangeStage)
trfBind = trfLocNoSema trfBind'

trfBind' :: TransformName n r => HsBind n -> Trf (AST.UValueBind (Dom r) RangeStage)
-- a value binding (not a function)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_pats = [], m_grhss = GRHSs [L _ (GRHS [] expr)] (L _ locals) })]} })
  = AST.USimpleBind <$> copyAnnot AST.UVarPat (define $ trfName id)
                    <*> addEmptyScope (addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr)))
                    <*> addEmptyScope (trfWhereLocalBinds (getLoc expr) locals)
trfBind' (FunBind id (MG (unLoc -> matches) _ _ _) _ _ _)
  = AST.UFunBind <$> makeNonemptyIndentedList (mapM (trfMatch (unLoc id)) matches)
trfBind' (PatBind pat (GRHSs rhs (unLoc -> locals)) _ _ _)
  = AST.USimpleBind <$> trfPattern pat
                    <*> addEmptyScope (addToScope locals (trfRhss rhs))
                    <*> addEmptyScope (trfWhereLocalBinds (collectLocs rhs) locals)
trfBind' (PatSynBind _) = error "Pattern synonym bindings should be recognized on the declaration level"
trfBind' b = unhandledElement "binding" b

trfMatch :: TransformName n r => n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.UMatch (Dom r) RangeStage)
trfMatch id = trfLocNoSema (trfMatch' id)

trfMatch' :: TransformName n r => n -> Match n (LHsExpr n) -> Trf (AST.UMatch (Dom r) RangeStage)
trfMatch' name (Match funid pats typ (GRHSs rhss (unLoc -> locBinds)))
  -- TODO: add the optional typ to pats
  = AST.UMatch <$> trfMatchLhs name funid pats
               <*> addToScope pats (addToScope locBinds (trfRhss rhss))
               <*> addToScope pats (trfWhereLocalBinds (collectLocs rhss) locBinds)

trfMatchLhs :: TransformName n r => n -> MatchFixity n -> [LPat n] -> Trf (Ann AST.UMatchLhs (Dom r) RangeStage)
trfMatchLhs name fb pats
  = do implicitIdLoc <- mkSrcSpan <$> atTheStart <*> atTheStart
       parenOpLoc <- tokensLoc [AnnOpenP, AnnVal, AnnCloseP]
       nonFunOpLoc <- tokenLoc AnnVal
       let infixLoc = case (parenOpLoc, nonFunOpLoc) of
                        (RealSrcSpan rsp1, RealSrcSpan rsp2)
                          | srcLocCol (realSrcSpanStart rsp2) == srcLocCol (realSrcSpanStart rsp1) + 1
                              && srcLocCol (realSrcSpanEnd rsp2) == srcLocCol (realSrcSpanEnd rsp1) - 1 -> parenOpLoc
                        _ -> nonFunOpLoc -- sometimes parenOpLoc is not an actual operator in parentheses, it just grabs
                                         -- a paren, so we need to check that it is actually what we seek
       closeLoc <- srcSpanStart <$> (combineSrcSpans <$> tokenLoc AnnEqual <*> tokenLoc AnnVbar)
       args <- mapM trfPattern pats
       let (n, isInfix) = case fb of NonFunBindMatch -> let token = if isSymOcc (occName name) && isGoodSrcSpan infixLoc then infixLoc else implicitIdLoc
                                                         in (L token name, length pats > 0 && srcSpanStart token >= srcSpanEnd (getLoc (pats !! 0)))
                                     FunBindMatch n inf -> (n, inf)
       annLocNoSema (mkSrcSpan <$> atTheStart <*> (pure closeLoc)) $
        case (args, isInfix) of
           (left:right:rest, True) -> AST.UInfixLhs left <$> define (trfOperator n) <*> pure right <*> makeList " " (pure closeLoc) (pure rest)
           _                       -> AST.UNormalLhs <$> define (trfName n) <*> makeList " " (pure closeLoc) (pure args)

trfRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.URhs (Dom r) RangeStage)
-- the original location on the GRHS misleadingly contains the local bindings
trfRhss [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> tokenBefore (srcSpanStart $ getLoc body) AnnEqual)
                                         (AST.UUnguardedRhs <$> trfExpr body)
trfRhss rhss = annLocNoSema (pure $ collectLocs rhss)
                      (AST.UGuardedRhss . nonemptyAnnList <$> mapM trfGuardedRhs rhss)

trfGuardedRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedRhs (Dom r) RangeStage)
trfGuardedRhs = trfLocNoSema $ \(GRHS guards body)
  -> AST.UGuardedRhs . nonemptyAnnList <$> trfScopedSequence trfRhsGuard guards <*> addToScope guards (trfExpr body)

trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard = trfLocNoSema trfRhsGuard'

trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' (BindStmt pat body _ _ _) = AST.UGuardBind <$> trfPattern pat <*> trfExpr body
trfRhsGuard' (BodyStmt body _ _ _) = AST.UGuardCheck <$> trfExpr body
trfRhsGuard' (LetStmt (unLoc -> binds)) = AST.UGuardLet <$> trfLocalBinds AnnLet binds
trfRhsGuard' d = unhandledElement "guard" d

trfWhereLocalBinds :: TransformName n r => SrcSpan -> HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfWhereLocalBinds _ EmptyLocalBinds = nothing "" "" atTheEnd
trfWhereLocalBinds bef binds
  = makeJust <$> annLocNoSema (combineSrcSpans (srcLocSpan (srcSpanEnd bef) `combineSrcSpans` getBindLocs binds) <$> tokenLocBack AnnWhere)
                              (AST.ULocalBinds <$> addToScope binds (trfLocalBinds AnnWhere binds))

getBindLocs :: HsLocalBinds n -> SrcSpan
getBindLocs (HsValBinds (ValBindsIn binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
getBindLocs (HsValBinds (ValBindsOut binds sigs)) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
getBindLocs (HsIPBinds (IPBinds binds _)) = foldLocs $ map getLoc binds
getBindLocs EmptyLocalBinds = noSrcSpan

trfLocalBinds :: TransformName n r => AnnKeywordId -> HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfLocalBinds token (HsValBinds (ValBindsIn binds sigs))
  = makeIndentedListBefore " " (after token)
      (orderDefs <$> ((++) <$> mapM (copyAnnot AST.ULocalValBind . trfBind) (bagToList binds)
                           <*> mapM trfLocalSig sigs))
trfLocalBinds token (HsValBinds (ValBindsOut binds sigs))
  = makeIndentedListBefore " " (after token)
      (orderDefs <$> ((++) <$> (concat <$> mapM (mapM (copyAnnot AST.ULocalValBind . trfBind) . bagToList . snd) binds)
                           <*> mapM trfLocalSig sigs))
trfLocalBinds token (HsIPBinds (IPBinds binds _))
  = makeIndentedListBefore " " (after token) (mapM trfIpBind binds)
trfLocalBinds _ b = unhandledElement "local binds" b

trfIpBind :: TransformName n r => Located (IPBind n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfIpBind = trfLocNoSema $ \case
  IPBind (Left (L l ipname)) expr
    -> AST.ULocalValBind
         <$> (annContNoSema $ AST.USimpleBind <$> focusOn l (annContNoSema (AST.UVarPat <$> define (trfImplicitName ipname)))
                                              <*> annFromNoSema AnnEqual (AST.UUnguardedRhs <$> trfExpr expr)
                                              <*> nothing " " "" atTheEnd)
  IPBind (Right _) _ -> error "trfIpBind: called on typechecked AST"

trfLocalSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfLocalSig = trfLocNoSema $ \case
  ts@(TypeSig {}) -> AST.ULocalSignature <$> annContNoSema (trfTypeSig' ts)
  (FixSig fs) -> AST.ULocalFixity <$> annContNoSema (trfFixitySig fs)
  (InlineSig name prag) -> AST.ULocalInline <$> trfInlinePragma name prag
  d -> unhandledElement "local signature" d

trfTypeSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig = trfLocNoSema trfTypeSig'

trfTypeSig' :: TransformName n r => Sig n -> Trf (AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig' (TypeSig names typ)
  = defineTypeVars $ AST.UTypeSignature <$> makeNonemptyList ", " (mapM trfName names) <*> trfType (hswc_body $ hsib_body typ)
trfTypeSig' ts = unhandledElement "type signature" ts

trfFixitySig :: TransformName n r => FixitySig n -> Trf (AST.UFixitySignature (Dom r) RangeStage)
trfFixitySig (FixitySig names (Fixity _ prec dir))
  = do precLoc <- tokenLoc AnnVal -- the precedence token or one of the names
       AST.UFixitySignature <$> transformDir dir
                            <*> (if isGoodSrcSpan precLoc && all (srcSpanEnd precLoc <) (map (srcSpanStart . getLoc) names)
                                   then makeJust <$> (annLocNoSema (return precLoc) $ pure $ AST.Precedence prec)
                                                                                         -- names cannot be empty
                                   else nothing "" " " (return $ srcSpanStart $ getLoc $ head names))
                            <*> (nonemptyAnnList . nub <$> mapM trfOperator names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLocNoSema (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)

        directionChar = annLocNoSema ((\l -> mkSrcSpan (updateCol (subtract 1) l) l) . srcSpanEnd <$> tokenLoc AnnInfix)

trfInlinePragma :: TransformName n r => Located n -> InlinePragma -> Trf (Ann AST.UInlinePragma (Dom r) RangeStage)
trfInlinePragma name (InlinePragma _ Inlinable _ phase _)
  = annContNoSema (AST.UInlinablePragma <$> trfPhase (pure $ srcSpanStart $ getLoc name) phase <*> trfName name)
trfInlinePragma name (InlinePragma src NoInline _ _ cl) = annContNoSema (AST.UNoInlinePragma <$> trfName name)
trfInlinePragma name (InlinePragma src Inline _ phase cl)
  = annContNoSema $ do rng <- asks contRange
                       let parts = map getLoc $ splitLocated (L rng src)
                       AST.UInlinePragma <$> trfConlike parts cl
                                         <*> trfPhase (pure $ srcSpanStart (getLoc name)) phase
                                         <*> trfName name

trfPhase :: Trf SrcLoc -> Activation -> Trf (AnnMaybeG AST.UPhaseControl (Dom r) RangeStage)
trfPhase l AlwaysActive = nothing " " "" l
trfPhase _ (ActiveAfter _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
                                                          (AST.UPhaseControl <$> nothing "" "" (before AnnCloseS) <*> (makeJust <$> trfPhaseNum pn))
trfPhase _ (ActiveBefore _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
                                                           (AST.UPhaseControl <$> (makeJust <$> annLocNoSema (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> (makeJust <$> trfPhaseNum pn))
trfPhase _ NeverActive = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
                                                   (AST.UPhaseControl <$> (makeJust <$> annLocNoSema (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> nothing " " "" (after AnnTilde))

trfPhaseNum ::  PhaseNum -> Trf (Ann AST.PhaseNumber (Dom r) RangeStage)
trfPhaseNum i = annLocNoSema (tokenLoc AnnVal) $ pure (AST.PhaseNumber $ fromIntegral i)

trfConlike :: [SrcSpan] -> RuleMatchInfo -> Trf (AnnMaybeG AST.UConlikeAnnot (Dom r) RangeStage)
trfConlike parts ConLike | length parts > 2
  = makeJust <$> annLocNoSema (pure $ parts !! 2) (pure AST.UConlikeAnnot)
  | otherwise = error $ "trfConlike: expected 3 parts, got: " ++ show parts
trfConlike (_:inlTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd inlTok)
trfConlike (combTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd combTok)
