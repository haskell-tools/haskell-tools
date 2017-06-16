{-# LANGUAGE TemplateHaskell #-}
module TH.NestedSplices where

import Language.Haskell.TH

f = [t| $(parensT [t| $(return ListT) |]) |]
