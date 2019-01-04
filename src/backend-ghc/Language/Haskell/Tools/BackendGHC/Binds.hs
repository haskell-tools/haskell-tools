{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that convert the value and function definitions of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Binds where

import ApiAnnotation as GHC (AnnKeywordId(..))
import Bag as GHC (bagToList)
import BasicTypes as GHC
import HsBinds as GHC
import HsExpr as GHC
import HsPat as GHC (LPat)
import HsTypes as GHC (SrcStrictness(..), HsWildCardBndrs(..), HsImplicitBndrs(..))
import Name as GHC (isSymOcc)
import PlaceHolder as GHC (NameOrRdrName)
import SrcLoc as GHC
import HsExtension (GhcPass, IdP)

import Control.Monad.Reader (Monad(..), mapM, asks)
import Data.List
import Data.Function (on)

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)
import Language.Haskell.Tools.BackendGHC.Utils

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfBind :: (TransformName n r, n ~ GhcPass p) => Located (HsBind n) -> Trf (Ann AST.UValueBind (Dom r) RangeStage)
trfBind = trfLocNoSema trfBind'

trfBind' :: forall n r p . (TransformName n r, n ~ GhcPass p) => HsBind n -> Trf (AST.UValueBind (Dom r) RangeStage)
-- A value binding with a strcitness annotation
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_ctxt = FunRhs { mc_strictness = SrcStrict }, m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] expr)] (L _ locals) })]} })
  = do bangLoc <- focusBeforeLoc (srcSpanStart $ getLoc id) $ tokenLoc AnnBang
       AST.USimpleBind <$> annLocNoSema (pure $ combineSrcSpans bangLoc (getLoc id))
                             (AST.UBangPat <$> copyAnnot AST.UVarPat (define $ trfName @n id))
                       <*> addEmptyScope (addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr)))
                       <*> addEmptyScope (trfWhereLocalBinds (getLoc expr) locals)
-- A value binding (not a function)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = L _ [L _ (Match { m_pats = [], m_grhss = GRHSs _ [L _ (GRHS _ [] expr)] (L _ locals) })]} })
  = AST.USimpleBind <$> copyAnnot AST.UVarPat (define $ trfName @n id)
                    <*> addEmptyScope (addToScope locals (annLocNoSema (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UUnguardedRhs <$> trfExpr expr)))
                    <*> addEmptyScope (trfWhereLocalBinds (getLoc expr) locals)
trfBind' (FunBind _ id (MG _ (unLoc -> matches) _) _ _)
  = AST.UFunBind <$> makeNonemptyIndentedList (mapM (trfMatch (unLoc id)) matches)
trfBind' (PatBind _ pat (GRHSs _ rhs (unLoc -> locals)) _)
  = AST.USimpleBind <$> trfPattern pat
                    <*> addEmptyScope (addToScope locals (trfRhss rhs))
                    <*> addEmptyScope (trfWhereLocalBinds (collectLocs rhs) locals)
trfBind' (PatSynBind _ _) = convertionProblem "Pattern synonym bindings should be recognized on the declaration level"
trfBind' b = unhandledElement "binding" b

trfMatch :: (TransformName n r, n ~ GhcPass p) => IdP n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.UMatch (Dom r) RangeStage)
trfMatch id = trfLocNoSema (trfMatch' id)

trfMatch' :: (TransformName n r, n ~ GhcPass p) => IdP n -> Match n (LHsExpr n) -> Trf (AST.UMatch (Dom r) RangeStage)
trfMatch' name (Match _ funid pats (GRHSs _ rhss (unLoc -> locBinds)))
  -- TODO: add the optional typ to pats
  = AST.UMatch <$> trfMatchLhs name funid pats
               <*> addToScope pats (addToScope locBinds (trfRhss rhss))
               <*> addToScope pats (trfWhereLocalBinds (collectLocs rhss) locBinds)

trfMatchLhs :: forall n r p . (TransformName n r, n ~ GhcPass p) => IdP n -> HsMatchContext (NameOrRdrName (IdP n)) -> [LPat n] -> Trf (Ann AST.UMatchLhs (Dom r) RangeStage)
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
       let (n, isInfix) = case fb of FunRhs n inf _ -> (n, inf == Infix)
                                     _ -> let token = if isSymOcc (occName @n name) && isGoodSrcSpan infixLoc then infixLoc else implicitIdLoc
                                           in (L token name, length pats > 0 && srcSpanStart token >= srcSpanEnd (getLoc (pats !! 0)))
       annLocNoSema (mkSrcSpan <$> atTheStart <*> (pure closeLoc)) $
        case (args, isInfix) of
           (left:right:rest, True) -> AST.UInfixLhs left <$> define (trfOperator @n n) <*> pure right <*> makeList " " (pure closeLoc) (pure rest)
           _                       -> AST.UNormalLhs <$> define (trfName @n n) <*> makeList " " (pure closeLoc) (pure args)

trfRhss :: (TransformName n r, n ~ GhcPass p) => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.URhs (Dom r) RangeStage)
-- the original location on the GRHS misleadingly contains the local bindings
trfRhss [unLoc -> GRHS _ [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> tokenBefore (srcSpanStart $ getLoc body) AnnEqual)
                                                 (AST.UUnguardedRhs <$> trfExpr body)
trfRhss rhss = annLocNoSema (pure $ collectLocs rhss)
                      (AST.UGuardedRhss . nonemptyAnnList <$> mapM trfGuardedRhs rhss)

trfGuardedRhs :: (TransformName n r, n ~ GhcPass p) => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedRhs (Dom r) RangeStage)
trfGuardedRhs = trfLocNoSema $ \(GRHS _ guards body)
  -> AST.UGuardedRhs . nonemptyAnnList <$> trfScopedSequence trfRhsGuard guards <*> addToScope guards (trfExpr body)

trfRhsGuard :: (TransformName n r, n ~ GhcPass p) => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard = trfLocNoSema trfRhsGuard'

trfRhsGuard' :: (TransformName n r, n ~ GhcPass p) => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' (BindStmt _ pat body _ _) = AST.UGuardBind <$> trfPattern pat <*> trfExpr body
trfRhsGuard' (BodyStmt _ body _ _) = AST.UGuardCheck <$> trfExpr body
trfRhsGuard' (LetStmt _ (unLoc -> binds)) = AST.UGuardLet <$> trfLocalBinds AnnLet binds
trfRhsGuard' d = unhandledElement "guard" d

trfWhereLocalBinds :: (TransformName n r, n ~ GhcPass p) => SrcSpan -> HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfWhereLocalBinds _ (EmptyLocalBinds _) = nothing "" "" atTheEnd
trfWhereLocalBinds bef binds
  = makeJust <$> annLocNoSema (combineSrcSpans (srcLocSpan (srcSpanEnd bef) `combineSrcSpans` getBindLocs binds) <$> tokenLocBack AnnWhere)
                              (AST.ULocalBinds <$> addToScope binds (trfLocalBinds AnnWhere binds))

getBindLocs :: n ~ GhcPass p => HsLocalBinds n -> SrcSpan
getBindLocs (HsValBinds _ (ValBinds _ binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
getBindLocs (HsValBinds _ (XValBindsLR (NValBinds binds sigs))) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
getBindLocs (HsIPBinds _ (IPBinds _ binds)) = foldLocs $ map getLoc binds
getBindLocs (EmptyLocalBinds _) = noSrcSpan

trfLocalBinds :: (TransformName n r, n ~ GhcPass p) => AnnKeywordId -> HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfLocalBinds token (HsValBinds _ (ValBinds _ binds sigs))
  = makeIndentedListBefore " " (after token)
      (orderDefs <$> ((++) <$> mapM (copyAnnot AST.ULocalValBind . trfBind) (bagToList binds)
                           <*> mapM trfLocalSig sigs))
trfLocalBinds token (HsValBinds _ (XValBindsLR (NValBinds binds sigs)))
  = makeIndentedListBefore " " (after token)
      (orderDefs <$> ((++) <$> (concat <$> mapM (mapM (copyAnnot AST.ULocalValBind . trfBind) . bagToList . snd) binds)
                           <*> mapM trfLocalSig sigs))
trfLocalBinds token (HsIPBinds _ (IPBinds _ binds))
  = makeIndentedListBefore " " (after token) (mapM trfIpBind binds)
trfLocalBinds _ b = unhandledElement "local binds" b

trfIpBind :: (TransformName n r, n ~ GhcPass p) => Located (IPBind n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfIpBind = trfLocNoSema $ \case
  IPBind _ (Left (L l ipname)) expr
    -> AST.ULocalValBind
         <$> (annContNoSema $ AST.USimpleBind <$> focusOn l (annContNoSema (AST.UVarPat <$> define (trfImplicitName ipname)))
                                              <*> annFromNoSema AnnEqual (AST.UUnguardedRhs <$> trfExpr expr)
                                              <*> nothing " " "" atTheEnd)
  IPBind _ (Right _) _ -> convertionProblem "trfIpBind: called on typechecked AST"

trfLocalSig :: forall n r p . (TransformName n r, n ~ GhcPass p) => Located (Sig n) -> Trf (Ann AST.ULocalBind (Dom r) RangeStage)
trfLocalSig = trfLocNoSema $ \case
  ts@(TypeSig {}) -> AST.ULocalSignature <$> annContNoSema (trfTypeSig' ts)
  (FixSig _ fs) -> AST.ULocalFixity <$> annContNoSema (trfFixitySig fs)
  (InlineSig _ name prag) -> AST.ULocalInline <$> trfInlinePragma @n name prag
  d -> unhandledElement "local signature" d

trfTypeSig :: (TransformName n r, n ~ GhcPass p) => Located (Sig n) -> Trf (Ann AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig = trfLocNoSema trfTypeSig'

trfTypeSig' :: forall n r p . (TransformName n r, n ~ GhcPass p) => Sig n -> Trf (AST.UTypeSignature (Dom r) RangeStage)
trfTypeSig' (TypeSig _ names typ)
  = defineTypeVars $ AST.UTypeSignature <$> makeNonemptyList ", " (mapM (trfName @n) names) <*> trfType (hsib_body $ hswc_body typ)
trfTypeSig' ts = unhandledElement "type signature" ts

trfFixitySig :: forall n r . TransformName n r => FixitySig n -> Trf (AST.UFixitySignature (Dom r) RangeStage)
trfFixitySig (FixitySig _ names (Fixity _ prec dir))
  = do precLoc <- tokenLoc AnnVal -- the precedence token or one of the names
       AST.UFixitySignature <$> transformDir dir
                            <*> (if isGoodSrcSpan precLoc && all (srcSpanEnd precLoc <) (map (srcSpanStart . getLoc) names)
                                   then makeJust <$> (annLocNoSema (return precLoc) $ pure $ AST.Precedence prec)
                                                                                         -- names cannot be empty
                                   else nothing "" " " (return $ srcSpanStart $ getLoc $ head names))
                            <*> (nonemptyAnnList . nubBy ((==) `on` AST.getRange) <$> mapM (trfOperator @n) names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLocNoSema (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)

        directionChar = annLocNoSema ((\l -> mkSrcSpan (updateCol (subtract 1) l) l) . srcSpanEnd <$> tokenLoc AnnInfix)

trfInlinePragma :: forall n r . TransformName n r => Located (IdP n) -> InlinePragma -> Trf (Ann AST.UInlinePragma (Dom r) RangeStage)
trfInlinePragma name (InlinePragma _ Inlinable _ phase _)
  = annContNoSema (AST.UInlinablePragma <$> trfPhase (pure $ srcSpanStart $ getLoc name) phase <*> trfName @n name)
trfInlinePragma name (InlinePragma _ NoInline _ _ _) = annContNoSema (AST.UNoInlinePragma <$> trfName @n name)
trfInlinePragma name (InlinePragma (fromSrcText -> src) Inline _ phase cl)
  = annContNoSema $ do rng <- asks contRange
                       let parts = map getLoc $ splitLocated (L rng src)
                       AST.UInlinePragma <$> trfConlike parts cl
                                         <*> trfPhase (pure $ srcSpanStart (getLoc name)) phase
                                         <*> trfName @n name

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
  | otherwise = convertionProblem $ "trfConlike: expected 3 parts, got: " ++ show parts
trfConlike (_:inlTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd inlTok)
trfConlike (combTok:_) FunLike = nothing " " "" (pure $ srcSpanEnd combTok)
