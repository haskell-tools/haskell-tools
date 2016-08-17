{-# LANGUAGE TemplateHaskell
           , DeriveDataTypeable
           , RecordWildCards
           , TypeFamilies
           , FlexibleInstances
           #-}
-- | The range template is an intermediate annotation level, where the children nodes of the tree
-- had been cut from the parent nodes, but the annotations still contain ranges instead of text.
module Language.Haskell.Tools.AnnTrf.RangeTemplate where

import Data.Data
import Control.Reference
import SrcLoc
import Language.Haskell.Tools.AST

instance SourceInfo RngTemplateStage where
  data SpanInfo RngTemplateStage = RangeTemplateNode { _rngTemplateNodeRange :: RealSrcSpan
                                                     , _rngTemplateNodeElems :: [RangeTemplateElem] 
                                                     }
    deriving Data
  data ListInfo RngTemplateStage = RangeTemplateList { _rngTemplateListRange :: RealSrcSpan
                                                     , _rngTmpListBefore :: String -- ^ Text that should be put before the first element if the list becomes populated
                                                     , _rngTmpListAfter :: String -- ^ Text that should be put after the last element if the list becomes populated
                                                     , _rngTmpDefaultSeparator :: String -- ^ The default separator if the list were empty
                                                     , _rngTmpIndented :: Bool -- ^ True, if the elements need to be aligned in the same column
                                                     , _rngTmpSeparators :: [RealSrcSpan] -- ^ The actual separators that were found in the source code
                                                     }
    deriving Data
  data OptionalInfo RngTemplateStage = RangeTemplateOpt { _rngTemplateOptRange :: RealSrcSpan
                                                        , _rngTmpOptBefore :: String -- ^ Text that should be put before the element if it appears
                                                        , _rngTmpOptAfter :: String -- ^ Text that should be put after the element if it appears
                                                        }
    deriving Data


rngTemplateNodeRange :: Simple Lens (SpanInfo RngTemplateStage) RealSrcSpan
rngTemplateNodeRange = lens _rngTemplateNodeRange (\v s -> s { _rngTemplateNodeRange = v })

rngTemplateNodeElems :: Simple Lens (SpanInfo RngTemplateStage) [RangeTemplateElem]
rngTemplateNodeElems = lens _rngTemplateNodeElems (\v s -> s { _rngTemplateNodeElems = v })

rngTemplateListRange :: Simple Lens (ListInfo RngTemplateStage) RealSrcSpan
rngTemplateListRange = lens _rngTemplateListRange (\v s -> s { _rngTemplateListRange = v })

rngTmpListBefore :: Simple Lens (ListInfo RngTemplateStage) String
rngTmpListBefore = lens _rngTmpListBefore (\v s -> s { _rngTmpListBefore = v })

rngTmpListAfter :: Simple Lens (ListInfo RngTemplateStage) String
rngTmpListAfter = lens _rngTmpListAfter (\v s -> s { _rngTmpListAfter = v })

rngTmpDefaultSeparator :: Simple Lens (ListInfo RngTemplateStage) String
rngTmpDefaultSeparator = lens _rngTmpDefaultSeparator (\v s -> s { _rngTmpDefaultSeparator = v })

rngTmpIndented :: Simple Lens (ListInfo RngTemplateStage) Bool
rngTmpIndented = lens _rngTmpIndented (\v s -> s { _rngTmpIndented = v })

rngTmpSeparators :: Simple Lens (ListInfo RngTemplateStage) [RealSrcSpan]
rngTmpSeparators = lens _rngTmpSeparators (\v s -> s { _rngTmpSeparators = v })

rngTemplateOptRange :: Simple Lens (OptionalInfo RngTemplateStage) RealSrcSpan
rngTemplateOptRange = lens _rngTemplateOptRange (\v s -> s { _rngTemplateOptRange = v })

rngTmpOptBefore :: Simple Lens (OptionalInfo RngTemplateStage) String
rngTmpOptBefore = lens _rngTmpOptBefore (\v s -> s { _rngTmpOptBefore = v })

rngTmpOptAfter :: Simple Lens (OptionalInfo RngTemplateStage) String
rngTmpOptAfter = lens _rngTmpOptAfter (\v s -> s { _rngTmpOptAfter = v })

-- | An element of a range template for a singleton AST node.
data RangeTemplateElem = RangeElem RealSrcSpan -- ^ A range for the source code of the element
                       | RangeChildElem        -- ^ The place for a child element
                       deriving Data

getRangeElemSpan :: RangeTemplateElem -> Maybe RealSrcSpan
getRangeElemSpan (RangeElem sp) = Just sp
getRangeElemSpan _ = Nothing

instance HasRange (SpanInfo RngTemplateStage) where 
  getRange = RealSrcSpan . (^. rngTemplateNodeRange)
  setRange (RealSrcSpan sp) = rngTemplateNodeRange .= sp
  setRange _ = id

instance HasRange (ListInfo RngTemplateStage) where 
  getRange = RealSrcSpan . (^. rngTemplateListRange)    
  setRange (RealSrcSpan sp) = rngTemplateListRange .= sp  
  setRange _ = id

instance HasRange (OptionalInfo RngTemplateStage) where 
  getRange = RealSrcSpan . (^. rngTemplateOptRange)
  setRange (RealSrcSpan sp) = rngTemplateOptRange .= sp  
  setRange _ = id

instance Show (SpanInfo RngTemplateStage) where
  show rngNode = concatMap show $ rngNode ^. rngTemplateNodeElems
instance Show (ListInfo RngTemplateStage) where
  show RangeTemplateList{..} = "«*" ++ shortShowSpan (RealSrcSpan _rngTemplateListRange) ++ " " ++ show _rngTmpListBefore ++ " " ++ show _rngTmpDefaultSeparator ++ " " ++ show _rngTmpListAfter ++ "*»"
instance Show (OptionalInfo RngTemplateStage) where
  show RangeTemplateOpt{..} = "«?" ++ shortShowSpan (RealSrcSpan _rngTemplateOptRange) ++ " " ++ show _rngTmpOptBefore ++ " " ++ show _rngTmpOptAfter ++ "?»"
                       
instance Show RangeTemplateElem where
  show (RangeElem sp) = shortShowSpan (RealSrcSpan sp)
  show RangeChildElem = "«.»"
