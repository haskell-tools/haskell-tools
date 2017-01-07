{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.ValBracket where

import TH.Splice.Define

data A = B

$(nameOf 'B)