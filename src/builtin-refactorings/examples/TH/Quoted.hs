{-# LANGUAGE TemplateHaskell #-}
module TH.Quoted where

import qualified Text.Read.Lex (Lexeme)

$(let x = ''Text.Read.Lex.Lexeme in return [])
