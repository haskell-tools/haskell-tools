{-# LANGUAGE TemplateHaskell #-}
module TH.DoubleSplice where

$($(id [| return [] |]))
