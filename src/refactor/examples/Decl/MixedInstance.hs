{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Decl.MixedInstance where

data Canvas = Canvas
data V2 = V2

class Backend c v e where
  data Options c v e :: *

instance Backend Canvas V2 Double where
  data Options Canvas V2 Double = CanvasOptions
