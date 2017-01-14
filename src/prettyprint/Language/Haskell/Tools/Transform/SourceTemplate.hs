{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , DeriveDataTypeable
           , TemplateHaskell
           , RecordWildCards
           , TypeFamilies
           #-}
-- | The final version of the source annotation. Each node contains its original textual format, with the places of 
-- the children specified by placeholders.
module Language.Haskell.Tools.Transform.SourceTemplate where

import Control.Reference
import Data.Data
import Language.Haskell.Tools.AST
import SrcLoc

instance SourceInfo SrcTemplateStage where
  data SpanInfo SrcTemplateStage 
         = SourceTemplateNode { _sourceTemplateNodeRange :: SrcSpan -- ^ The (original) range of the given element
                              , _sourceTemplateNodeElems :: [SourceTemplateElem] -- ^ The children of the given node, could be text or child nodes
                              , _srcTmpMinInd :: Int -- ^ Minimum indentation for the element
                              , _srcTmpRelPos :: Maybe Int -- ^ Relative indentation for newly created elements
                              }
    deriving (Eq, Ord, Data)
  data ListInfo SrcTemplateStage 
         = SourceTemplateList { _sourceTemplateListRange :: SrcSpan -- ^ The (original) range of the given element
                              , _srcTmpListBefore :: String -- ^ Text that should be put before the first element if the list becomes populated
                              , _srcTmpListAfter :: String -- ^ Text that should be put after the last element if the list becomes populated
                              , _srcTmpDefaultSeparator :: String -- ^ The default separator if the list were empty
                              , _srcTmpIndented :: Bool -- ^ True, if the elements need to be aligned in the same column
                              , _srcTmpSeparators :: [String] -- ^ The actual separators that were found in the source code
                              , _srcTmpListMinInd :: Int -- ^ Minimum indentation for the element
                              , _srcTmpListRelPos :: Maybe Int -- ^ Relative indentation for newly created elements
                              }
    deriving (Eq, Ord, Data)
  data OptionalInfo SrcTemplateStage 
         = SourceTemplateOpt { _sourceTemplateOptRange :: SrcSpan -- ^ The (original) range of the given element
                             , _srcTmpOptBefore :: String -- ^ Text that should be put before the element if it appears
                             , _srcTmpOptAfter :: String -- ^ Text that should be put after the element if it appears
                             , _srcTmpOptMinInd :: Int -- ^ Minimum indentation for the element
                             , _srcTmpOptRelPos :: Maybe Int -- ^ Relative indentation for newly created elements
                             }
    deriving (Eq, Ord, Data)

-- * References for span infos

sourceTemplateNodeRange :: Simple Lens (SpanInfo SrcTemplateStage) SrcSpan
sourceTemplateNodeRange = lens _sourceTemplateNodeRange (\v s -> s { _sourceTemplateNodeRange = v })

sourceTemplateNodeElems :: Simple Lens (SpanInfo SrcTemplateStage) [SourceTemplateElem]
sourceTemplateNodeElems = lens _sourceTemplateNodeElems (\v s -> s { _sourceTemplateNodeElems = v })

sourceTemplateMinimalIndent :: Simple Lens (SpanInfo SrcTemplateStage) Int
sourceTemplateMinimalIndent = lens _srcTmpMinInd (\v s -> s { _srcTmpMinInd = v })

srcTmpRelPos :: Simple Lens (SpanInfo SrcTemplateStage) (Maybe Int)
srcTmpRelPos = lens _srcTmpRelPos (\v s -> s { _srcTmpRelPos = v })

-- * References for list infos

sourceTemplateListRange :: Simple Lens (ListInfo SrcTemplateStage) SrcSpan
sourceTemplateListRange = lens _sourceTemplateListRange (\v s -> s { _sourceTemplateListRange = v })

srcTmpListBefore :: Simple Lens (ListInfo SrcTemplateStage) String
srcTmpListBefore = lens _srcTmpListBefore (\v s -> s { _srcTmpListBefore = v })

srcTmpListAfter :: Simple Lens (ListInfo SrcTemplateStage) String
srcTmpListAfter = lens _srcTmpListAfter (\v s -> s { _srcTmpListAfter = v })

srcTmpDefaultSeparator :: Simple Lens (ListInfo SrcTemplateStage) String
srcTmpDefaultSeparator = lens _srcTmpDefaultSeparator (\v s -> s { _srcTmpDefaultSeparator = v })

srcTmpIndented :: Simple Lens (ListInfo SrcTemplateStage) Bool
srcTmpIndented = lens _srcTmpIndented (\v s -> s { _srcTmpIndented = v })

srcTmpSeparators :: Simple Lens (ListInfo SrcTemplateStage) [String]
srcTmpSeparators = lens _srcTmpSeparators (\v s -> s { _srcTmpSeparators = v })

srcTmpListMinimalIndent :: Simple Lens (ListInfo SrcTemplateStage) Int
srcTmpListMinimalIndent = lens _srcTmpListMinInd (\v s -> s { _srcTmpListMinInd = v })

srcTmpListRelPos :: Simple Lens (ListInfo SrcTemplateStage) (Maybe Int)
srcTmpListRelPos = lens _srcTmpListRelPos (\v s -> s { _srcTmpListRelPos = v })

-- * References for optional infos

sourceTemplateOptRange :: Simple Lens (OptionalInfo SrcTemplateStage) SrcSpan
sourceTemplateOptRange = lens _sourceTemplateOptRange (\v s -> s { _sourceTemplateOptRange = v })

srcTmpOptBefore :: Simple Lens (OptionalInfo SrcTemplateStage) String
srcTmpOptBefore = lens _srcTmpOptBefore (\v s -> s { _srcTmpOptBefore = v })

srcTmpOptAfter :: Simple Lens (OptionalInfo SrcTemplateStage) String
srcTmpOptAfter = lens _srcTmpOptAfter (\v s -> s { _srcTmpOptAfter = v })

srcTmpOptMinimalIndent :: Simple Lens (OptionalInfo SrcTemplateStage) Int
srcTmpOptMinimalIndent = lens _srcTmpOptMinInd (\v s -> s { _srcTmpOptMinInd = v })
      
srcTmpOptRelPos :: Simple Lens (OptionalInfo SrcTemplateStage) (Maybe Int)
srcTmpOptRelPos = lens _srcTmpOptRelPos (\v s -> s { _srcTmpOptRelPos = v })
      

-- | An element of a source template for a singleton AST node.
data SourceTemplateElem
  = TextElem { _sourceTemplateText :: String } -- ^ Source text belonging to the current node
  | ChildElem -- ^ Placeholder for the next children of the node
     deriving (Eq, Ord, Data)

makeReferences ''SourceTemplateElem

instance HasRange (SpanInfo SrcTemplateStage) where 
  getRange = (^. sourceTemplateNodeRange)      
  setRange = (sourceTemplateNodeRange .=) 

instance HasRange (ListInfo SrcTemplateStage) where 
  getRange = (^. sourceTemplateListRange)      
  setRange = (sourceTemplateListRange .=) 
  
instance HasRange (OptionalInfo SrcTemplateStage) where 
  getRange = (^. sourceTemplateOptRange)
  setRange = (sourceTemplateOptRange .=) 
      
instance Show (SpanInfo SrcTemplateStage) where
  show (SourceTemplateNode _ sp _ _) = concatMap show sp
instance Show (ListInfo SrcTemplateStage) where
  show SourceTemplateList{..} = "<*" ++ show _srcTmpListBefore ++ " " ++ show _srcTmpDefaultSeparator ++ " " ++ show _srcTmpListAfter ++ "*>"
instance Show (OptionalInfo SrcTemplateStage) where
  show SourceTemplateOpt{..} = "<?" ++ show _srcTmpOptBefore ++ " " ++ show _srcTmpOptAfter ++ "?>"

instance Show SourceTemplateElem where
  show (TextElem s) = s
  show ChildElem = "<.>"

