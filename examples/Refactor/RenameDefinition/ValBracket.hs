{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.ValBracket where

import TH.Splice.Define

data A = A

$(nameOf 'A)