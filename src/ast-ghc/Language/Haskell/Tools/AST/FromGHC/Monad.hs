-- | The transformation monad carries the range in focus and the src map that
-- contains the tokens in the file.
module Language.Haskell.Tools.AST.FromGHC.Monad where

import SrcLoc
import GHC
import ApiAnnotation
import Control.Monad.Reader
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Data.Map as Map
import Data.Maybe

-- | The (immutable) data for the transformation
data TrfInput
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , pragmaComms :: Map String [Located String] -- ^ Pragma comments
             , contRange :: SrcSpan -- ^ The focus of the transformation
             , localsInScope :: [[GHC.Name]] -- ^ Local names visible
             , defining :: Bool
             , originalNames :: Map SrcSpan RdrName
             }
      
trfInit :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> TrfInput
trfInit annots comments 
  = TrfInput { srcMap = annotationsToSrcMap annots
             , pragmaComms = comments
             , contRange = noSrcSpan
             , localsInScope = []
             , defining = False
             , originalNames = empty
             }

define :: Trf a -> Trf a
define = local (\s -> s { defining = True })

addToScope :: HsHasName e => e -> Trf a -> Trf a
addToScope e = local (\s -> s { localsInScope = hsGetNames e : localsInScope s }) 

addToCurrentScope :: HsHasName e => e -> Trf a -> Trf a
addToCurrentScope e = local (\s -> s { localsInScope = case localsInScope s of lastScope:rest -> (hsGetNames e ++ lastScope):rest })

-- | Performs the transformation given the tokens of the source file
runTrf :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> Trf a -> Ghc a
runTrf annots comments trf = runReaderT trf (trfInit annots comments)

setOriginalNames :: Map SrcSpan RdrName -> Trf a -> Trf a
setOriginalNames names = local (\s -> s { originalNames = names })

getOriginalName :: RdrName -> Trf String
getOriginalName n = do sp <- asks contRange
                       asks (rdrNameStr . fromMaybe n . (Map.lookup sp) . originalNames)
                          
type Trf = ReaderT TrfInput Ghc