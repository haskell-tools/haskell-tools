{-# LANGUAGE TemplateHaskell, TupleSections #-}
module A where

$(let x = (3,) in return [])