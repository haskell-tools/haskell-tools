{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that convert the pattern-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Patterns where

import ApiAnnotation as GHC (AnnKeywordId(..))
import BasicTypes as GHC (Boxity(..))
import Data.List
import HsExpr (HsSplice(..))
import HsLit as GHC (HsOverLit(..))
import HsPat as GHC
import HsTypes as GHC (HsConDetails(..))
import Language.Haskell.Tools.BackendGHC.GHCUtils (getFieldOccName)
import SrcLoc as GHC
import Control.Monad.Reader
import HsExtension (GhcPass)

import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.BackendGHC.Literals
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..), trfOperator, trfName)
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.TH (trfSplice, trfQuasiQuotation')
import Language.Haskell.Tools.BackendGHC.Utils

import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfPattern :: forall n r p . (TransformName n r, n ~ GhcPass p) => Located (Pat n) -> Trf (Ann AST.UPattern (Dom r) RangeStage)
-- field wildcards are not directly represented in GHC AST
trfPattern (L l (ConPatIn name (RecCon (HsRecFields flds _)))) | any ((l ==) . getLoc) flds
  = focusOn l $ do
      let (fromWC, notWC) = partition ((l ==) . getLoc) flds
      normalFields <- mapM (trfLocNoSema trfPatternField') notWC
      wildc <- annLocNoSema (tokenLocBack AnnDotdot) (AST.UFieldWildcardPattern <$> annCont (createImplicitFldInfo (unLoc . (\(VarPat _ n) -> n) . unLoc) (map unLoc fromWC)) (pure AST.FldWildcard))
      annLocNoSema (pure l) (AST.URecPat <$> trfName @n name <*> makeNonemptyList ", " (pure (normalFields ++ [wildc])))
trfPattern p = trfLocNoSema trfPattern' (correctPatternLoc p)

-- | Locations for right-associative infix patterns are incorrect in GHC AST
correctPatternLoc :: Located (Pat n) -> Located (Pat n)
correctPatternLoc (L _ p@(ConPatIn _ (InfixCon left right)))
  = L (getLoc (correctPatternLoc left) `combineSrcSpans` getLoc (correctPatternLoc right)) p
correctPatternLoc p = p

trfPattern' :: forall n r p . (TransformName n r, n ~ GhcPass p) => Pat n -> Trf (AST.UPattern (Dom r) RangeStage)
trfPattern' (WildPat _) = pure AST.UWildPat
trfPattern' (VarPat _ name) = define $ AST.UVarPat <$> trfName @n name
trfPattern' (LazyPat _ pat) = AST.UIrrefutablePat <$> trfPattern pat
trfPattern' (AsPat _ name pat) = AST.UAsPat <$> define (trfName @n name) <*> trfPattern pat
trfPattern' (ParPat _ pat) = AST.UParenPat <$> trfPattern pat
trfPattern' (BangPat _ pat) = AST.UBangPat <$> trfPattern pat
trfPattern' (ListPat _ pats) = AST.UListPat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (TuplePat _ pats Boxed) = AST.UTuplePat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (TuplePat _ pats Unboxed) = AST.UUnboxTuplePat <$> makeList ", " atTheEnd (mapM trfPattern pats)
trfPattern' (ConPatIn name (PrefixCon args)) = AST.UAppPat <$> trfName @n name <*> makeList " " atTheEnd (mapM trfPattern args)
trfPattern' (ConPatIn name (RecCon (HsRecFields flds _))) = AST.URecPat <$> trfName @n name <*> trfAnnList ", " trfPatternField' flds
trfPattern' (ConPatIn name (InfixCon left right)) = AST.UInfixAppPat <$> trfPattern left <*> trfOperator @n name <*> trfPattern right
trfPattern' (ViewPat _ expr pat) = AST.UViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat _ qq@(HsQuasiQuote {})) = AST.UQuasiQuotePat <$> annContNoSema (trfQuasiQuotation' qq)
trfPattern' (SplicePat _ splice) = AST.USplicePat <$> trfSplice splice
trfPattern' (LitPat _ lit) = AST.ULitPat <$> annCont (pure $ RealLiteralInfo (monoLiteralType lit)) (trfLiteral' lit)
trfPattern' (NPat _ (ol_val . unLoc -> lit) _ _) = AST.ULitPat <$> annCont (asks contRange >>= pure . PreLiteralInfo) (trfOverloadedLit lit)
trfPattern' (NPlusKPat _ id (L l lit) _ _ _) = AST.UNPlusKPat <$> define (trfName @n id) <*> annLoc (asks contRange >>= pure . PreLiteralInfo) (pure l) (trfOverloadedLit (ol_val lit))
trfPattern' (CoPat _ _ pat _) = trfPattern' pat -- coercion pattern introduced by GHC
trfPattern' (SumPat _ pat tag arity)
  = do sepsBefore <- focusBeforeLoc (srcSpanStart (getLoc pat)) (eachTokenLoc (AnnOpen : replicate (tag - 1) AnnVbar))
       sepsAfter <- focusAfterLoc (srcSpanEnd (getLoc pat)) (eachTokenLoc (replicate (arity - tag) AnnVbar))
       let locsBefore = map srcSpanEnd $ init sepsBefore
           locsAfter = map srcSpanEnd sepsAfter
       AST.UUnboxedSumPat <$> makeList " | " (after AnnOpen) (mapM makePlaceholder locsBefore)
                          <*> trfPattern pat
                          <*> makeList " | " (before AnnClose) (mapM makePlaceholder locsAfter)
  where makePlaceholder l = annLocNoSema (pure (srcLocSpan l)) (pure AST.UUnboxedSumPlaceHolder)
trfPattern' p = unhandledElement "pattern" p

trfPatternField' :: forall n r p . (TransformName n r, n ~ GhcPass p) => HsRecField n (LPat n) -> Trf (AST.UPatternField (Dom r) RangeStage)
trfPatternField' (HsRecField id arg False) = AST.UNormalFieldPattern <$> trfName @n (getFieldOccName id) <*> trfPattern arg
trfPatternField' (HsRecField id _ True) = AST.UFieldPunPattern <$> trfName @n (getFieldOccName id)
