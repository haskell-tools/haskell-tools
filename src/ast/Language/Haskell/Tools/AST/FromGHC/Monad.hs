module Language.Haskell.Tools.AST.FromGHC.Monad where

import SrcLoc
import GHC
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

runTrf :: Map ApiAnnKey [SrcSpan] -> Trf a -> Ghc a
runTrf annots trf = runReaderT trf (trfInit annots)
                          
type Trf = ReaderT TrfInput Ghc