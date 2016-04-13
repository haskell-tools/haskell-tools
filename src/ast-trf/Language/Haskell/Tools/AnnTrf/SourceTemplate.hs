{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , DeriveDataTypeable
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.AnnTrf.SourceTemplate where

import Data.Data
import Data.String
import Control.Reference
import SrcLoc
import Language.Haskell.Tools.AST

data SourceTemplateElem 
  = TextElem String 
  | ChildElem
  | OptionalChildElem { _srcTmpBefore :: String
                      , _srcTmpAfter :: String
                      }
  | ChildListElem { _srcTmpBefore :: String
                  , _srcTmpAfter :: String
                  , _srcTmpDefaultSeparator :: String
                  , _srcTmpIndented :: Bool
                  , _srcTmpSeparators :: [String] 
                  }
     deriving (Eq, Ord, Data)
     
makeReferences ''SourceTemplateElem

-- | A pattern that controls how the original source code can be
-- retrieved from the AST. A source template is assigned to each node.
-- It has holes where the content of an other node should be printed.
data SourceTemplate = SourceTemplate { _sourceTemplateRange :: SrcSpan
                                     , _sourceTemplateElems :: [SourceTemplateElem] 
                                     } deriving Data 

makeReferences ''SourceTemplate
      
instance HasRange (NodeInfo sema SourceTemplate) where 
  getRange = (^. sourceInfo&sourceTemplateRange)
      
instance Show SourceTemplateElem where
  show (TextElem s) = s
  show (ChildElem) = "«.»"
  show (OptionalChildElem {}) = "«?»"
  show (ChildListElem {}) = "«*»"

instance Show SourceTemplate where
  show (SourceTemplate rng sp) = concatMap show sp
