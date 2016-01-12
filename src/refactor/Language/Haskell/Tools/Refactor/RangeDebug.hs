{-# LANGUAGE TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           #-}
module Language.Haskell.Tools.Refactor.RangeDebug where

import GHC.Generics
import SrcLoc

rangeDebug :: RangeDebug e => e SrcSpan -> String
rangeDebug = rangeDebug' 0
      
class RangeDebug e where
  rangeDebug' :: Int -> e SrcSpan -> String
  default rangeDebug' :: (GRangeDebug (Rep (e SrcSpan)), Generic (e SrcSpan)) => Int -> e SrcSpan -> String
  rangeDebug' i = gRangeDebug i . from

class GRangeDebug f where 
  gRangeDebug :: Int -> f p -> String
  
instance GRangeDebug V1 where
  gRangeDebug i _ = undefined
  
instance GRangeDebug U1 where
  gRangeDebug i U1 = ""  
  
instance (GRangeDebug f, GRangeDebug g) => GRangeDebug (f :+: g) where
  gRangeDebug i (L1 x) = gRangeDebug i x
  gRangeDebug i (R1 x) = gRangeDebug i x
  
instance (GRangeDebug f, GRangeDebug g) => GRangeDebug (f :*: g) where
  gRangeDebug i (x :*: y) 
    = gRangeDebug i x ++ gRangeDebug i y

instance {-# OVERLAPPING #-} RangeDebug c => GRangeDebug (K1 i (c SrcSpan)) where
  gRangeDebug i (K1 x) = rangeDebug' i x
  
instance {-# OVERLAPPABLE #-} GRangeDebug (K1 i c) where
  gRangeDebug i (K1 x) = ""
        
instance GRangeDebug f => GRangeDebug (M1 i t f) where
  gRangeDebug i (M1 x) = gRangeDebug i x
  
shortShowSpan :: SrcSpan -> String
shortShowSpan (RealSrcSpan sp) 
  = show (srcSpanStartLine sp) ++ ":" ++ show (srcSpanStartCol sp) 
      ++ "-" ++ show (srcSpanEndLine sp) ++ ":" ++ show (srcSpanEndCol sp)
shortShowSpan _ = "???"