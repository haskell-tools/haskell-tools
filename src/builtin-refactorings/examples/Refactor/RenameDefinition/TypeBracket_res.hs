{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.TypeBracket where

import TH.Splice.Define

data B = A

nameOf ''B