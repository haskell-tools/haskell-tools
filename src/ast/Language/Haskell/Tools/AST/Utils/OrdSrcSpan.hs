-- | A wrapper for SrcSpans that is ordered.
module Language.Haskell.Tools.AST.Utils.OrdSrcSpan where

import FastString (FastString)
import SrcLoc

-- | Wraps the SrcSpan into an ordered source span
ordSrcSpan :: SrcSpan -> OrdSrcSpan
ordSrcSpan (RealSrcSpan sp) = OrdSrcSpan sp
ordSrcSpan (UnhelpfulSpan fs) = NoOrdSrcSpan fs

-- | Unwrap the ordered source span
fromOrdSrcSpan :: OrdSrcSpan -> SrcSpan 
fromOrdSrcSpan (OrdSrcSpan sp) = RealSrcSpan sp
fromOrdSrcSpan (NoOrdSrcSpan fs) = UnhelpfulSpan fs

-- | A wrapper for SrcSpans that is ordered.
data OrdSrcSpan 
  = OrdSrcSpan RealSrcSpan
  | NoOrdSrcSpan FastString
  deriving (Show, Eq)

instance Ord OrdSrcSpan where
  compare (NoOrdSrcSpan _) (NoOrdSrcSpan _) = EQ
  compare (OrdSrcSpan _) (NoOrdSrcSpan _) = GT
  compare (NoOrdSrcSpan _) (OrdSrcSpan _) = LT
  compare (OrdSrcSpan rsp1)  (OrdSrcSpan rsp2) 
    = compare (realSrcSpanStart rsp1) (realSrcSpanStart rsp2)
        `mappend` compare (realSrcSpanEnd rsp1) (realSrcSpanEnd rsp2)
        