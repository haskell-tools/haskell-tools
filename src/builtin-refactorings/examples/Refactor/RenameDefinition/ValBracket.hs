{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.ValBracket where

import Refactor.RenameDefinition.ThHelper

data A = A

$(nameOf 'A)