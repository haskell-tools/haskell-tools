{-# LANGUAGE ParallelListComp, 
             TransformListComp,
             MonadComprehensions,
             RecordWildCards #-}
module Expr.GeneralizedListComp where

import GHC.Exts
import qualified Data.Map as M
import Data.Ord (comparing)

data Character = Character
  { firstName :: String
  , lastName :: String
  , birthYear :: Int
  } deriving (Show, Eq)

friends :: [Character]
friends = [ Character "Phoebe" "Buffay" 1963
          , Character "Chandler" "Bing" 1969
          , Character "Rachel" "Green" 1969
          , Character "Joey" "Tribbiani" 1967
          , Character "Monica" "Geller" 1964
          , Character "Ross" "Geller" 1966
          ]
          
oldest :: Int -> [Character] -> [String]
oldest k tbl = [ firstName ++ " " ++ lastName
               | Character{..} <- tbl
               , then sortWith by birthYear
               , then take k
               ]