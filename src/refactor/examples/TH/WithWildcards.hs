{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TH.WithWildcards where

import Language.Haskell.TH.Syntax

data A = A { x :: Q Exp }

g A{..} = [| $(x) |]
