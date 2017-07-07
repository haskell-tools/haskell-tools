module Refactor.GenerateTypeSignature.BringToScope.AlreadyQualImport where

import Refactor.GenerateTypeSignature.BringToScope.B
import qualified Refactor.GenerateTypeSignature.BringToScope.A as AAA (S)
import qualified Refactor.GenerateTypeSignature.BringToScope.A(T)

g :: Refactor.GenerateTypeSignature.BringToScope.A.T -> AAA.S
g = f
