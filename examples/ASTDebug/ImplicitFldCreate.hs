{-# LANGUAGE RecordWildCards #-}
module ASTDebug.ImplicitFldCreate where

data M = M { a :: Int }

x = let a = 3 in M { .. }
