-- | The transformation monad carries the range in focus and the src map that
-- contains the tokens in the file.
module Language.Haskell.Tools.AST.FromGHC.Monad where

import SrcLoc
import GHC
import ApiAnnotation
import Control.Monad.Reader
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Data.Map

-- | The (immutable) data for the transformation
data TrfInput 
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , contRange :: SrcSpan -- ^ The focus of the transformation
             }
      
trfInit :: Map ApiAnnKey [SrcSpan] -> TrfInput 
trfInit annots = TrfInput { srcMap = annotationsToSrcMap annots
                          , contRange = noSrcSpan
                          }

-- | Performs the transformation given the tokens of the source file
runTrf :: Map ApiAnnKey [SrcSpan] -> Trf a -> Ghc a
runTrf annots trf = runReaderT trf (trfInit annots)
                          
type Trf = ReaderT TrfInput Ghc