module Language.Haskell.Tools.AST.FromGHC.OrdSrcSpan where

import SrcLoc
import FastString

ordSrcSpan :: SrcSpan -> OrdSrcSpan
ordSrcSpan (RealSrcSpan sp) = OrdSrcSpan sp
ordSrcSpan (UnhelpfulSpan fs) = NoOrdSrcSpan fs

fromOrdSrcSpan :: OrdSrcSpan -> SrcSpan 
fromOrdSrcSpan (OrdSrcSpan sp) = RealSrcSpan sp
fromOrdSrcSpan (NoOrdSrcSpan fs) = UnhelpfulSpan fs

data OrdSrcSpan 
  = OrdSrcSpan RealSrcSpan
  | NoOrdSrcSpan FastString
  deriving Eq

instance Ord OrdSrcSpan where
  compare (NoOrdSrcSpan _) (NoOrdSrcSpan _) = EQ
  compare (OrdSrcSpan _) (NoOrdSrcSpan _) = GT
  compare (NoOrdSrcSpan _) (OrdSrcSpan _) = LT
  compare (OrdSrcSpan rsp1)  (OrdSrcSpan rsp2) 
    = compare (realSrcSpanStart rsp1) (realSrcSpanStart rsp2)
        `mappend` compare (realSrcSpanEnd rsp1) (realSrcSpanEnd rsp2)
        