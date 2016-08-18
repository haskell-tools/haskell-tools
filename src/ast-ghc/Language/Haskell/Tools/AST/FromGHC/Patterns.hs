{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , AllowAmbiguousTypes
           #-}
module Language.Haskell.Tools.AST.FromGHC.Patterns where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsPat as GHC
import HsLit as GHC
import ApiAnnotation as GHC
import BasicTypes as GHC
import Unique as GHC
import Debug.Trace
import Data.List
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

import Language.Haskell.Tools.AST.FromGHC.Base
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfPattern :: TransformName n r => Located (Pat n) -> Trf (Ann AST.Pattern (Dom r) RangeStage)
-- field wildcards are not directly represented in GHC AST
trfPattern (L l (ConPatIn name (RecCon (HsRecFields flds _)))) | any ((l ==) . getLoc) flds 
  = do let (fromWC, notWC) = partition ((l ==) . getLoc) flds
       normalFields <- mapM (trfLocNoSema trfPatternField') notWC
       wildc <- annLocNoSema (tokenLoc AnnDotdot) (AST.FieldWildcardPattern <$> annCont (createImplicitFldInfo (unLoc . (\(VarPat n) -> n) . unLoc) (map unLoc fromWC)) (pure AST.FldWildcard))
       annLocNoSema (pure l) (AST.RecPat <$> trfName name <*> makeNonemptyList ", " (pure (normalFields ++ [wildc])))
trfPattern p | otherwise = trfLocNoSema trfPattern' (correctPatternLoc p)

-- | Locations for right-associative infix patterns are incorrect in GHC AST
correctPatternLoc :: Located (Pat n) -> Located (Pat n)
correctPatternLoc (L l p@(ConPatIn name (InfixCon left right)))
  = L (getLoc (correctPatternLoc left) `combineSrcSpans` getLoc (correctPatternLoc right)) p
correctPatternLoc p = p

trfPattern' :: TransformName n r => Pat n -> Trf (AST.Pattern (Dom r) RangeStage)
trfPattern' (WildPat _) = pure AST.WildPat
trfPattern' (VarPat name) = define $ AST.VarPat <$> trfName name
trfPattern' (LazyPat pat) = AST.IrrPat <$> trfPattern pat
trfPattern' (AsPat name pat) = AST.AsPat <$> define (trfName name) <*> trfPattern pat
trfPattern' (ParPat pat) = AST.ParenPat <$> trfPattern pat
trfPattern' (BangPat pat) = AST.BangPat <$> trfPattern pat
trfPattern' (ListPat pats _ _) = AST.ListPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (TuplePat pats Boxed _) = AST.TuplePat <$> trfAnnList ", " trfPattern' pats
trfPattern' (PArrPat pats _) = AST.ParArrPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (ConPatIn name (PrefixCon args)) = AST.AppPat <$> trfName name <*> trfAnnList " " trfPattern' args
trfPattern' (ConPatIn name (RecCon (HsRecFields flds _))) = AST.RecPat <$> trfName name <*> trfAnnList ", " trfPatternField' flds
trfPattern' (ConPatIn name (InfixCon left right)) = AST.InfixPat <$> trfPattern left <*> trfOperator name <*> trfPattern right
trfPattern' (ViewPat expr pat _) = AST.ViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat splice) = AST.SplicePat <$> annContNoSema (trfSplice' splice)
trfPattern' (LitPat lit) = AST.LitPat <$> annContNoSema (trfLiteral' lit)
trfPattern' (SigPatIn pat (hswc_body . hsib_body -> typ)) = AST.TypeSigPat <$> trfPattern pat <*> trfType typ
trfPattern' (NPat (ol_val . unLoc -> lit) _ _ _) = AST.LitPat <$> annContNoSema (trfOverloadedLit lit)
trfPattern' (NPlusKPat id (L l lit) _ _ _ _) = AST.NPlusKPat <$> define (trfName id) <*> annLocNoSema (pure l) (trfOverloadedLit (ol_val lit))
-- coercion pattern introduced by GHC
trfPattern' (CoPat _ pat _) = trfPattern' pat

trfPatternField' :: TransformName n r => HsRecField n (LPat n) -> Trf (AST.PatternField (Dom r) RangeStage)
trfPatternField' (HsRecField id arg False) = AST.NormalFieldPattern <$> trfName (getFieldOccName id) <*> trfPattern arg
trfPatternField' (HsRecField id _ True) = AST.FieldPunPattern <$> trfName (getFieldOccName id)