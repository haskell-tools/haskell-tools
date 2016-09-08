{-# LANGUAGE TemplateHaskell #-}
module Use where

import Define

defHello

$(let x = return [] in x)