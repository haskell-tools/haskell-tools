{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.TypeBracket where

import TH.Splice.Define

data A = A

nameOf ''A