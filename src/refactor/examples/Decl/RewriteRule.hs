module Decl.RewriteRule where

{-# RULES "map/map" forall f g xs . map f (map g xs) = map (f . g) xs #-}
