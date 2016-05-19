{-# LANGUAGE UnicodeSyntax #-}
module Expr.UnicodeSyntax where
    
import Data.List.Unicode ((∪))

main ∷ IO ()
main = print $ [1, 2, 3] ∪ [1, 3, 5]
