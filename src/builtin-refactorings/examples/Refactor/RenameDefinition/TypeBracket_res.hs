{-# LANGUAGE TemplateHaskell #-}
module Refactor.RenameDefinition.TypeBracket where

import Refactor.RenameDefinition.ThHelper

data B = A

nameOf ''B