-- | The transformation monad carries the necessary information that is passed top-down
-- during the conversion from GHC AST to our representation.
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
import Language.Haskell.Tools.AST

import Debug.Trace

-- | The transformation monad type
type Trf = ReaderT TrfInput Ghc

-- | The (immutable) data for the transformation
data TrfInput
  = TrfInput { srcMap :: SourceMap -- ^ The lexical tokens of the source file
             , pragmaComms :: Map String [Located String] -- ^ Pragma comments
             , declsToInsert :: [Ann Decl (Dom RdrName) RangeStage] -- ^ Declarations that are from the parsed AST
             , contRange :: SrcSpan -- ^ The focus of the transformation
             , localsInScope :: [[GHC.Name]] -- ^ Local names visible
             , defining :: Bool -- ^ True, if names are defined in the transformed AST element.
             , definingTypeVars :: Bool -- ^ True, if type variable names are defined in the transformed AST element.
             , originalNames :: Map SrcSpan RdrName -- ^ Stores the original format of names.
             , spliceLocs :: [Located (HsSplice GHC.Name)] -- ^ Location of the TH splices for extracting the locations from the renamed AST. 
             }
      
trfInit :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> TrfInput
trfInit annots comments 
  = TrfInput { srcMap = annotationsToSrcMap annots
             , pragmaComms = comments
             , declsToInsert = []
             , contRange = noSrcSpan
             , localsInScope = []
             , defining = False
             , definingTypeVars = False
             , originalNames = empty
             , spliceLocs = []
             }

-- | Perform the transformation taking names as defined.
define :: Trf a -> Trf a
define = local (\s -> s { defining = True })

-- | Perform the transformation taking type variable names as defined.
defineTypeVars :: Trf a -> Trf a
defineTypeVars = local (\s -> s { definingTypeVars = True })

-- | Transform as type variables
typeVarTransform :: Trf a -> Trf a
typeVarTransform = local (\s -> s { defining = defining s || definingTypeVars s })

-- | Transform a name as a type variable if it is one.
transformingPossibleVar :: HsHasName n => n -> Trf a -> Trf a
transformingPossibleVar n = case hsGetNames n of 
  [name] | isVarName name || isTyVarName name -> typeVarTransform
  _                                           -> id

-- | Perform the transformation putting the given definition in a new local scope.
addToScope :: HsHasName e => e -> Trf a -> Trf a
addToScope e = local (\s -> s { localsInScope = hsGetNames e : localsInScope s }) 

-- | Perform the transformation putting the given definitions in the current scope.
addToCurrentScope :: HsHasName e => e -> Trf a -> Trf a
addToCurrentScope e = local (\s -> s { localsInScope = case localsInScope s of lastScope:rest -> (hsGetNames e ++ lastScope):rest
                                                                               []             -> [hsGetNames e] })

-- | Performs the transformation given the tokens of the source file
runTrf :: Map ApiAnnKey [SrcSpan] -> Map String [Located String] -> Trf a -> Ghc a
runTrf annots comments trf = runReaderT trf (trfInit annots comments)

setOriginalNames :: Map SrcSpan RdrName -> Trf a -> Trf a
setOriginalNames names = local (\s -> s { originalNames = names })

-- | Get the original format of a name (before scoping).
getOriginalName :: RdrName -> Trf String
getOriginalName n = do sp <- asks contRange
                       asks (rdrNameStr . fromMaybe n . (Map.lookup sp) . originalNames)

setSpliceLocs :: [Located (HsSplice GHC.Name)] -> Trf a -> Trf a
setSpliceLocs locs = local (\s -> s {spliceLocs = locs})

setDeclsToInsert :: [Ann Decl (Dom RdrName) RangeStage] -> Trf a -> Trf a
setDeclsToInsert decls = local (\s -> s {declsToInsert = decls})
