-- | The transformation monad carries the range in focus and the src map that
-- contains the tokens in the file.
module Language.Haskell.Tools.AST.FromGHC.Monad where

import SrcLoc
import GHC
import Name
import ApiAnnotation
import Outputable (ppr, showSDocUnsafe)
import Control.Monad.Reader
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Data.Map as Map
import Data.Maybe

import Debug.Trace

-- | The (immutable) data for the transformation
data TrfInput
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , pragmaComms :: Map String [Located String] -- ^ Pragma comments
             , contRange :: SrcSpan -- ^ The focus of the transformation
             , localsInScope :: [[GHC.Name]] -- ^ Local names visible
             , defining :: Bool
             , definingTypeVars :: Bool
             , originalNames :: Map SrcSpan RdrName
             }
      
trfInit :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> TrfInput
trfInit annots comments 
  = TrfInput { srcMap = annotationsToSrcMap annots
             , pragmaComms = comments
             , contRange = noSrcSpan
             , localsInScope = []
             , defining = False
             , definingTypeVars = False
             , originalNames = empty
             }

define :: Trf a -> Trf a
define = local (\s -> s { defining = True })

defineTypeVars :: Trf a -> Trf a
defineTypeVars = local (\s -> s { definingTypeVars = True })

typeVarTransform :: Trf a -> Trf a
typeVarTransform = local (\s -> s { defining = defining s || definingTypeVars s })

transformingPossibleVar :: HsHasName n => n -> Trf a -> Trf a
transformingPossibleVar n = case hsGetNames n of 
  [name] | isVarName name || isTyVarName name -> typeVarTransform
  _                                           -> id

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