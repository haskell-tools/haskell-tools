{-# LANGUAGE RecordWildCards #-}
module ASTDebug.ImplicitFldExtract where

data M = M { a :: Int }

x (M { .. }) = a
