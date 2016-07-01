{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , DeriveDataTypeable
           , TemplateHaskell
           , RecordWildCards
           #-}
-- | The final version of the source annotation. Each node contains its original textual format, with the places of 
-- the children specified by placeholders.
module Language.Haskell.Tools.AnnTrf.SourceTemplate where

import Data.Data
import Data.String
import Control.Reference
import SrcLoc
import Language.Haskell.Tools.AST

data SourceTemplateElem 
  = TextElem String -- ^ Source text belonging to the current node
  | ChildElem -- ^ Placeholder for the next children of the node
  | OptionalChildElem { _srcTmpBefore :: String -- ^ Text that should be put before the element if it appears
                      , _srcTmpAfter :: String -- ^ Text that should be put after the element if it appears
                      }
  | ChildListElem { _srcTmpBefore :: String -- ^ Text that should be put before the first element if the list becomes populated
                  , _srcTmpAfter :: String -- ^ Text that should be put after the last element if the list becomes populated
                  , _srcTmpDefaultSeparator :: String -- ^ The default separator if the list were empty
                  , _srcTmpIndented :: Bool -- ^ True, if the elements need to be aligned in the same column
                  , _srcTmpSeparators :: [String] -- ^ The actual separators that were found in the source code
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
  show ChildElem = "«.»"
  show OptionalChildElem{..} = "«?" ++ show _srcTmpBefore ++ " " ++ show _srcTmpAfter ++ "?»"
  show ChildListElem{..} = "«*" ++ show _srcTmpBefore ++ " " ++ show _srcTmpDefaultSeparator ++ " " ++ show _srcTmpAfter ++ "*»"

instance Show SourceTemplate where
  show (SourceTemplate rng sp) = concatMap show sp
