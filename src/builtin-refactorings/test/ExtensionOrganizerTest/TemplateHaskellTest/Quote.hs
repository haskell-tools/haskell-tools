{-# LANGUAGE TemplateHaskellQuotes #-}

module Quote where

import Language.Haskell.TH

add1 :: Int -> Q Exp
add1 x = [| x + 1 |]  {-* TemplateHaskellQuotes *-}
