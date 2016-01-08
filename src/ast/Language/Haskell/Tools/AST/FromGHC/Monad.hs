module Language.Haskell.Tools.AST.FromGHC.Monad where

import SrcLoc
import ApiAnnotation
import Control.Monad.Reader
import Language.Haskell.Tools.AST.SourceMap
import Data.Map

data TrfInput = TrfInput { srcMap :: SourceMap
                         , contRange :: SrcSpan
                         }
      
trfInit :: Map ApiAnnKey [SrcSpan] -> TrfInput 
trfInit annots = TrfInput { srcMap = annotationsToSrcMap annots
                          , contRange = noSrcSpan
                          }

runTrf :: Map ApiAnnKey [SrcSpan] -> Trf a -> a
runTrf annots trf = runReader trf (trfInit annots)
                          
type Trf = Reader TrfInput