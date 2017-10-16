{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , AllowAmbiguousTypes
           #-}
-- | Functions that convert the pattern-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Patterns where

import ApiAnnotation as GHC (AnnKeywordId(..))
import BasicTypes as GHC (Boxity(..))
import Data.List
import HsExpr (HsSplice(..))
import HsLit as GHC (HsOverLit(..))
import HsPat as GHC
import HsTypes as GHC (HsWildCardBndrs(..), HsImplicitBndrs(..), HsConDetails(..))
import Language.Haskell.Tools.BackendGHC.GHCUtils (getFieldOccName)
import SrcLoc as GHC

import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.Literals (trfLiteral', trfOverloadedLit)
import Language.Haskell.Tools.BackendGHC.Monad (Trf, define)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..), trfOperator, trfName)
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.TH (trfSplice, trfQuasiQuotation')
import Language.Haskell.Tools.BackendGHC.Types (trfType)
import Language.Haskell.Tools.BackendGHC.Utils

import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfPattern :: TransformName n r => Located (Pat n) -> Trf (Ann AST.UPattern (Dom r) RangeStage)
-- field wildcards are not directly represented in GHC AST
trfPattern (L l (ConPatIn name (RecCon (HsRecFields flds _)))) | any ((l ==) . getLoc) flds
  = focusOn l $ do
      let (fromWC, notWC) = partition ((l ==) . getLoc) flds
      normalFields <- mapM (trfLocNoSema trfPatternField') notWC
      wildc <- annLocNoSema (tokenLocBack AnnDotdot) (AST.UFieldWildcardPattern <$> annCont (createImplicitFldInfo (unLoc . (\(VarPat n) -> n) . unLoc) (map unLoc fromWC)) (pure AST.FldWildcard))
      annLocNoSema (pure l) (AST.URecPat <$> trfName name <*> makeNonemptyList ", " (pure (normalFields ++ [wildc])))
trfPattern p = trfLocNoSema trfPattern' (correctPatternLoc p)

-- | Locations for right-associative infix patterns are incorrect in GHC AST
correctPatternLoc :: Located (Pat n) -> Located (Pat n)
correctPatternLoc (L _ p@(ConPatIn _ (InfixCon left right)))
  = L (getLoc (correctPatternLoc left) `combineSrcSpans` getLoc (correctPatternLoc right)) p
correctPatternLoc p = p

trfPattern' :: TransformName n r => Pat n -> Trf (AST.UPattern (Dom r) RangeStage)
trfPattern' (WildPat _) = pure AST.UWildPat
trfPattern' (VarPat name) = define $ AST.UVarPat <$> trfName name
trfPattern' (LazyPat pat) = AST.UIrrefutablePat <$> trfPattern pat
trfPattern' (AsPat name pat) = AST.UAsPat <$> define (trfName name) <*> trfPattern pat
trfPattern' (ParPat pat) = AST.UParenPat <$> trfPattern pat
trfPattern' (BangPat pat) = AST.UBangPat <$> trfPattern pat
trfPattern' (ListPat pats _ _) = AST.UListPat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (TuplePat pats Boxed _) = AST.UTuplePat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (TuplePat pats Unboxed _) = AST.UUnboxTuplePat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (PArrPat pats _) = AST.UParArrPat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (ConPatIn name (PrefixCon args)) = AST.UAppPat <$> trfName name <*> makeList " " atTheEnd (mapM trfPattern args)
trfPattern' (ConPatIn name (RecCon (HsRecFields flds _))) = AST.URecPat <$> trfName name <*> trfAnnList ", " trfPatternField' flds
trfPattern' (ConPatIn name (InfixCon left right)) = AST.UInfixAppPat <$> trfPattern left <*> trfOperator name <*> trfPattern right
trfPattern' (ViewPat expr pat _) = AST.UViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat qq@(HsQuasiQuote {})) = AST.UQuasiQuotePat <$> annContNoSema (trfQuasiQuotation' qq)
trfPattern' (SplicePat splice) = AST.USplicePat <$> trfSplice splice
trfPattern' (LitPat lit) = AST.ULitPat <$> annContNoSema (trfLiteral' lit)
trfPattern' (SigPatIn pat (hsib_body . hswc_body -> typ)) = AST.UTypeSigPat <$> trfPattern pat <*> trfType typ
trfPattern' (NPat (ol_val . unLoc -> lit) _ _ _) = AST.ULitPat <$> annContNoSema (trfOverloadedLit lit)
trfPattern' (NPlusKPat id (L l lit) _ _ _ _) = AST.UNPlusKPat <$> define (trfName id) <*> annLocNoSema (pure l) (trfOverloadedLit (ol_val lit))
trfPattern' (CoPat _ pat _) = trfPattern' pat -- coercion pattern introduced by GHC
trfPattern' (SumPat pat tag arity _)
  = do sepsBefore <- focusBeforeLoc (srcSpanStart (getLoc pat)) (eachTokenLoc (AnnOpen : replicate (tag - 1) AnnVbar))
       sepsAfter <- focusAfterLoc (srcSpanEnd (getLoc pat)) (eachTokenLoc (replicate (arity - tag) AnnVbar))
       let locsBefore = map srcSpanEnd $ init sepsBefore
           locsAfter = map srcSpanEnd sepsAfter
       AST.UUnboxedSumPat <$> makeList " | " (after AnnOpen) (mapM makePlaceholder locsBefore)
                          <*> trfPattern pat
                          <*> makeList " | " (before AnnClose) (mapM makePlaceholder locsAfter)
  where makePlaceholder l = annLocNoSema (pure (srcLocSpan l)) (pure AST.UUnboxedSumPlaceHolder)
trfPattern' p = unhandledElement "pattern" p

trfPatternField' :: TransformName n r => HsRecField n (LPat n) -> Trf (AST.UPatternField (Dom r) RangeStage)
trfPatternField' (HsRecField id arg False) = AST.UNormalFieldPattern <$> trfName (getFieldOccName id) <*> trfPattern arg
trfPatternField' (HsRecField id _ True) = AST.UFieldPunPattern <$> trfName (getFieldOccName id)
