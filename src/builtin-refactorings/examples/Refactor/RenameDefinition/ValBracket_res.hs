{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.ValBracket where

import Refactor.RenameDefinition.ThHelper

data A = B

$(nameOf 'B)