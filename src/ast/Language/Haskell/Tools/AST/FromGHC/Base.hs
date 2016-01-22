{-# LANGUAGE LambdaCase
           , TupleSections
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import HsSyn as GHC
import Module as GHC
import RdrName as GHC
import Name as GHC hiding (Name)
import Outputable as GHC
import SrcLoc as GHC
import BasicTypes as GHC
import FastString as GHC

import Language.Haskell.Tools.AST.Ann as AST
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Literals as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfName :: Located RdrName -> Trf (Ann Name RI)
trfName = trfLoc trfName'

trfName' :: RdrName -> Trf (Name RI)
trfName' n = AST.nameFromList . fst <$> trfNameStr (occNameString (rdrNameOcc n))

trfSimplName :: SrcLoc -> OccName -> Trf (Ann SimpleName RI)
trfSimplName start n = (\srcLoc -> Ann (mkSrcSpan start srcLoc) $ SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)

trfNameStr :: String -> Trf (AnnList SimpleName RI, SrcLoc)
trfNameStr str = (\srcLoc -> (\(ls,loc) -> (AnnList ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (mkSrcSpan loc nextLoc) (SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." str))) <$> asks (srcSpanStart . contRange)

trfLiteral' :: HsLit -> Trf (AST.Literal RI)
trfLiteral' (HsChar _ ch) = pure $ AST.CharLit ch
trfLiteral' (HsCharPrim _ ch) = pure $ AST.PrimCharLit ch
trfLiteral' (HsString _ str) = pure $ AST.StringLit (unpackFS str)
trfLiteral' (HsStringPrim _ str) = pure $ AST.PrimStringLit (BS.foldr (:) "" str)
trfLiteral' (HsInt _ i) = pure $ AST.IntLit i
trfLiteral' (HsIntPrim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsWordPrim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsInt64Prim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsWord64Prim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsInteger _ i _) = pure $ AST.PrimIntLit i
trfLiteral' (HsRat frac _) = pure $ AST.FracLit (fl_value frac)
trfLiteral' (HsFloatPrim frac) = pure $ AST.PrimFloatLit (fl_value frac)
trfLiteral' (HsDoublePrim frac) = pure $ AST.PrimDoubleLit (fl_value frac)
  
trfOverloadedLit :: OverLitVal -> Trf (AST.Literal RI)
trfOverloadedLit (HsIntegral _ i) = pure $ AST.IntLit i
trfOverloadedLit (HsFractional frac) = pure $ AST.FracLit (fl_value frac)
trfOverloadedLit (HsIsString _ str) = pure $ AST.StringLit (unpackFS str)
  
trfModuleName :: Located ModuleName -> Trf (Ann Name RI)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: ModuleName -> Trf (Name RI)
trfModuleName' = (AST.nameFromList . fst <$>) . trfNameStr . moduleNameString
  
-- * utility methods
  
advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
advanceAllSrcLoc oth _ = oth
  
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                