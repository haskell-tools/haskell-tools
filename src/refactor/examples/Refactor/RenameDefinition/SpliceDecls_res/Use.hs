{-# LANGUAGE TemplateHaskell #-}
module Use where

import Define

hello

$(let x = return [] in x)