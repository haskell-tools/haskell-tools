-- | Utility functions for transforming the GHC AST representation into our own.
{-# LANGUAGE TypeSynonymInstances 
           , FlexibleInstances
           , LambdaCase
           , ViewPatterns
           , MultiParamTypeClasses
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AST.FromGHC.Utils where

import ApiAnnotation
import SrcLoc
import GHC
import Avail
import HscTypes
import HsSyn
import Module
import Name
import Outputable
import FastString

import Control.Monad.Reader
import Control.Reference hiding (element)
import Data.Maybe
import Data.IORef
import Data.Function hiding ((&))
import Data.List
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Debug.Trace

-- | Annotations that is made up from ranges
class HasRange annot => RangeAnnot annot where
  toNodeAnnot :: SrcSpan -> annot
  toListAnnot :: String -> String -> String -> SrcLoc -> annot
  toIndentedListAnnot :: String -> String -> String -> SrcLoc -> annot
  toOptAnnot :: String -> String -> SrcLoc -> annot

instance RangeAnnot (NodeInfo (SemanticInfo n) SpanInfo) where
  toNodeAnnot = NodeInfo NoSemanticInfo . NodeSpan
  toListAnnot bef aft sep = NodeInfo NoSemanticInfo . ListPos bef aft sep False
  toIndentedListAnnot bef aft sep = NodeInfo NoSemanticInfo . ListPos bef aft sep True
  toOptAnnot bef aft = NodeInfo NoSemanticInfo . OptionalPos bef aft

data SemanticsPhantom n = SemanticsPhantom

-- | Annotations that carry semantic information
class SemanticAnnot annot n where
  addSemanticInfo :: SemanticInfo n -> annot -> annot
  addScopeData :: annot -> Trf annot
  addImportData :: SemanticsPhantom n -> Ann AST.ImportDecl annot -> Trf (Ann AST.ImportDecl annot)
  
instance SemanticAnnot RangeWithName GHC.Name where
  addSemanticInfo si = semanticInfo .= si
  addScopeData = semanticInfo !~ (\case NoSemanticInfo -> do locals <- asks localsInScope
                                                             return $ ScopeInfo locals
                                        inf -> return inf)
  addImportData _ = addImportData'

instance {-# OVERLAPPING #-} SemanticAnnot RangeInfo RdrName where
  addSemanticInfo si = semanticInfo .= si
  addScopeData = semanticInfo !~ (\case NoSemanticInfo -> do locals <- asks localsInScope
                                                             return $ ScopeInfo locals
                                        inf -> return inf)
  addImportData _ = pure

instance {-# OVERLAPPABLE #-} SemanticAnnot RangeInfo n where
  addSemanticInfo si = id
  addScopeData = pure
  addImportData _ = pure
  
-- | Adds semantic information to an impord declaration. See ImportInfo.
addImportData' :: Ann AST.ImportDecl RangeWithName -> Trf (Ann AST.ImportDecl RangeWithName)
addImportData' imp = lift $ 
  do eps <- getSession >>= liftIO . readIORef . hsc_EPS
     mod <- findModule (mkModuleName . nameString $ imp ^. element&importModule&element) 
                       (fmap mkFastString $ imp ^? element&importPkg&annJust&element&stringNodeStr)
     -- load exported names from interface file
     let ifaceNames = concatMap availNames $ maybe [] mi_exports 
                                           $ flip lookupModuleEnv mod 
                                           $ eps_PIT eps
     loadedNames <- maybe [] modInfoExports <$> getModuleInfo mod
     let importedNames = ifaceNames ++ loadedNames
     names <- filterM (checkImportVisible (imp ^. element)) importedNames
     return $ annotation .- addSemanticInfo (ImportInfo mod importedNames names) $ imp

checkImportVisible :: GhcMonad m => AST.ImportDecl RangeWithName -> GHC.Name -> m Bool
checkImportVisible imp name
  | importIsExact imp 
  = or <$> mapM (`ieSpecMatches` name) (imp ^? importExacts :: [IESpec RangeWithName])
  | importIsHiding imp 
  = not . or <$> mapM (`ieSpecMatches` name) (imp ^? importHidings :: [IESpec RangeWithName])
  | otherwise = return True

ieSpecMatches :: GhcMonad m => AST.IESpec RangeWithName -> GHC.Name -> m Bool
ieSpecMatches (AST.IESpec ((^? element&simpleName&annotation&semanticInfo&nameInfo) -> Just n) ss) name
  | n == name = return True
  | isTyConName n
  = (\case Just (ATyCon tc) -> name `elem` map getName (tyConDataCons tc)) 
             <$> lookupName n
  | otherwise = return False


-- | Creates a place for a missing node with a default location
nothing :: RangeAnnot a => String -> String -> Trf SrcLoc -> Trf (AnnMaybe e a)
nothing bef aft pos = annNothing . toOptAnnot bef aft <$> pos 

emptyList :: RangeAnnot a => String -> Trf SrcLoc -> Trf (AnnList e a)
emptyList sep ann = AnnList <$> (toListAnnot "" "" sep <$> ann) <*> pure []

-- | Creates a place for a list of nodes with a default place if the list is empty.
makeList :: RangeAnnot a => String -> Trf SrcLoc -> Trf [Ann e a] -> Trf (AnnList e a)
makeList sep ann ls = AnnList <$> (toListAnnot "" "" sep <$> ann) <*> ls

makeListBefore :: RangeAnnot a => String -> String -> Trf SrcLoc -> Trf [Ann e a] -> Trf (AnnList e a)
makeListBefore bef sep ann ls = do isEmpty <- null <$> ls 
                                   AnnList <$> (toListAnnot (if isEmpty then bef else "") "" sep <$> ann) <*> ls

makeListAfter :: RangeAnnot a => String -> String -> Trf SrcLoc -> Trf [Ann e a] -> Trf (AnnList e a)
makeListAfter aft sep ann ls = do isEmpty <- null <$> ls 
                                  AnnList <$> (toListAnnot "" (if isEmpty then aft else "") sep <$> ann) <*> ls

makeNonemptyList :: RangeAnnot a => String -> Trf [Ann e a] -> Trf (AnnList e a)
makeNonemptyList sep ls = AnnList (toListAnnot "" "" sep noSrcLoc) <$> ls

-- | Creates a place for an indented list of nodes with a default place if the list is empty.
makeIndentedList :: RangeAnnot a => Trf SrcLoc -> Trf [Ann e a] -> Trf (AnnList e a)
makeIndentedList ann ls = AnnList <$> (toIndentedListAnnot "" "" "\n" <$> ann) <*> ls

makeIndentedListNewlineBefore :: RangeAnnot a => Trf SrcLoc -> Trf [Ann e a] -> Trf (AnnList e a)
makeIndentedListNewlineBefore ann ls = do isEmpty <- null <$> ls 
                                          AnnList <$> (toIndentedListAnnot (if isEmpty then "\n" else "") "" "\n" <$> ann) <*> ls

makeNonemptyIndentedList :: RangeAnnot a => Trf [Ann e a] -> Trf (AnnList e a)
makeNonemptyIndentedList ls = AnnList (toIndentedListAnnot "" "" "\n" noSrcLoc) <$> ls
  
-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: RangeAnnot i => (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLoc = trfLocCorrect pure

-- | Transforms a possibly-missing node with the default location of the end of the focus.
trfMaybe :: RangeAnnot i => String -> String -> (Located a -> Trf (Ann e i)) -> Maybe (Located a) -> Trf (AnnMaybe e i)
trfMaybe bef aft f = trfMaybeDefault bef aft f atTheEnd

-- | Transforms a possibly-missing node with a default location
trfMaybeDefault :: RangeAnnot i => String -> String -> (Located a -> Trf (Ann e i)) -> Trf SrcLoc -> Maybe (Located a) -> Trf (AnnMaybe e i)
trfMaybeDefault _   _   f _   (Just e) = makeJust <$> f e
trfMaybeDefault bef aft _ loc Nothing  = nothing bef aft loc

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: RangeAnnot i => (SrcSpan -> Trf SrcSpan) -> (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLocCorrect locF f (L l e) = annLoc (locF l) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: RangeAnnot i => (a -> Trf (Maybe (b i))) -> Located a -> Trf (Maybe (Ann b i))
trfMaybeLoc f (L l e) = do fmap (Ann (toNodeAnnot l)) <$> local (\s -> s { contRange = l }) (f e)

-- | Creates a place for a list of nodes with the default place at the end of the focus if the list is empty.
trfAnnList :: RangeAnnot i => String -> (a -> Trf (b i)) -> [Located a] -> Trf (AnnList b i)
trfAnnList sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList sep f ls = makeList sep (pure $ noSrcLoc) (mapM (trfLoc f) ls)

-- | Creates a place for a list of nodes that cannot be empty.
nonemptyAnnList :: RangeAnnot i => [Ann e i] -> AnnList e i
nonemptyAnnList = AnnList (toListAnnot "" "" "" noSrcLoc)

-- | Creates an optional node from an existing element
makeJust :: RangeAnnot a => Ann e a -> AnnMaybe e a
makeJust e = AnnMaybe (toOptAnnot "" "" noSrcLoc) (Just e)

-- | Annotates a node with the given location and focuses on the given source span.
annLoc :: RangeAnnot a => Trf SrcSpan -> Trf (b a) -> Trf (Ann b a)
annLoc locm nodem = do loc <- locm
                       node <- local (\s -> s { contRange = loc }) nodem
                       return (Ann (toNodeAnnot loc) node)

-- * Focus manipulation

-- | Focuses the transformation to go between tokens. The tokens must be found inside the current range.
between :: AnnKeywordId -> AnnKeywordId -> Trf a -> Trf a
between firstTok lastTok = focusAfter firstTok . focusBefore lastTok

-- | Focuses the transformation to go between tokens if they are present
betweenIfPresent :: AnnKeywordId -> AnnKeywordId -> Trf a -> Trf a
betweenIfPresent firstTok lastTok = focusAfterIfPresent firstTok . focusBeforeIfPresent lastTok

-- | Focuses the transformation to be performed after the given token. The token must be found inside the current range.
focusAfter :: AnnKeywordId -> Trf a -> Trf a
focusAfter firstTok trf
  = do firstToken <- tokenLoc firstTok
       if (isGoodSrcSpan firstToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanEnd firstToken) (srcSpanEnd (contRange s))}) trf
          else do rng <- asks contRange 
                  error $ "focusAfter: token not found in " ++ show rng ++ ": " ++ show firstTok

focusAfterIfPresent :: AnnKeywordId -> Trf a -> Trf a
focusAfterIfPresent firstTok trf
  = do firstToken <- tokenLoc firstTok
       if (isGoodSrcSpan firstToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanEnd firstToken) (srcSpanEnd (contRange s))}) trf
          else trf

-- | Focuses the transformation to be performed after the given token. The token must be found inside the current range.
focusBefore :: AnnKeywordId -> Trf a -> Trf a
focusBefore lastTok trf
  = do lastToken <- tokenLocBack lastTok
       if (isGoodSrcSpan lastToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanStart lastToken)}) trf
          else do rng <- asks contRange 
                  error $ "focusBefore: token not found in " ++ show rng ++ ": " ++ show lastTok

focusBeforeIfPresent :: AnnKeywordId -> Trf a -> Trf a
focusBeforeIfPresent lastTok trf
  = do lastToken <- tokenLocBack lastTok
       if (isGoodSrcSpan lastToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanStart lastToken)}) trf
          else trf

-- | Gets the position before the given token
before :: AnnKeywordId -> Trf SrcLoc
before tok = srcSpanStart <$> tokenLoc tok
               
-- | Gets the position after the given token
after :: AnnKeywordId -> Trf SrcLoc
after tok = srcSpanEnd <$> tokenLoc tok

-- | The element should span from the given token to the end of focus
annFrom :: RangeAnnot a => AnnKeywordId -> Trf (e a) -> Trf (Ann e a)
annFrom kw = annLoc (combineSrcSpans <$> tokenLoc kw <*> asks (srcLocSpan . srcSpanEnd . contRange))

-- | Gets the position at the beginning of the focus       
atTheStart :: Trf SrcLoc
atTheStart = asks (srcSpanStart . contRange)
            
-- | Gets the position at the end of the focus      
atTheEnd :: Trf SrcLoc
atTheEnd = asks (srcSpanEnd . contRange)
                 
-- | Searches for a token inside the focus and retrieves its location
tokenLoc :: AnnKeywordId -> Trf SrcSpan
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

allTokenLoc :: AnnKeywordId -> Trf [SrcSpan]
allTokenLoc keyw = getKeywordsInside keyw <$> asks contRange <*> asks srcMap

-- | Searches for a token backward inside the focus and retrieves its location
tokenLocBack :: AnnKeywordId -> Trf SrcSpan
tokenLocBack keyw = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> asks contRange <*> asks srcMap)

tokenBefore :: SrcLoc -> AnnKeywordId -> Trf SrcSpan
tokenBefore loc keyw 
  = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> (mkSrcSpan <$> (asks (srcSpanStart . contRange)) <*> pure loc) <*> asks srcMap)

-- | Searches for tokens in the given order inside the parent element and returns their combined location
tokensLoc :: [AnnKeywordId] -> Trf SrcSpan
tokensLoc keys = asks contRange >>= tokensLoc' keys
  where tokensLoc' :: [AnnKeywordId] -> SrcSpan -> Trf SrcSpan
        tokensLoc' (keyw:rest) r 
          = do spanFirst <- tokenLoc keyw
               spanRest <- tokensLoc' rest (mkSrcSpan (srcSpanEnd spanFirst) (srcSpanEnd r))
               return (combineSrcSpans spanFirst spanRest)                   
        tokensLoc' [] r = pure noSrcSpan
        
-- | Searches for a token and retrieves its location anywhere
uniqueTokenAnywhere :: AnnKeywordId -> Trf SrcSpan
uniqueTokenAnywhere keyw = fromMaybe noSrcSpan <$> (getKeywordAnywhere keyw <$> asks srcMap)
        
-- | Annotates the given element with the current focus as a location.
annCont :: RangeAnnot a => Trf (e a) -> Trf (Ann e a)
annCont = annLoc (asks contRange)

-- | Annotates the element with the same annotation that is on the other element
copyAnnot :: (Ann a i -> b i) -> Trf (Ann a i) -> Trf (Ann b i)
copyAnnot f at = (\(Ann i a) -> Ann i (f (Ann i a))) <$> at

-- | Combine source spans into one that contains them all
foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

-- | Update column information in a source location
updateCol :: (Int -> Int) -> SrcLoc -> SrcLoc
updateCol f loc@(UnhelpfulLoc _) = loc
updateCol f (RealSrcLoc loc) = mkSrcLoc (srcLocFile loc) (srcLocLine loc) (f $ srcLocCol loc)

-- | Combine source spans of elements into one that contains them all
collectLocs :: [Located e] -> SrcSpan
collectLocs = foldLocs . map getLoc

-- | Rearrange definitions to appear in the order they are defined in the source file.
orderDefs :: RangeAnnot i => [Ann e i] -> [Ann e i]
orderDefs = sortBy (compare `on` AST.ordSrcSpan . getRange . _annotation)

-- | Orders a list of elements to the order they are defined in the source file.
orderAnnList :: RangeAnnot i => AnnList e i -> AnnList e i
orderAnnList (AnnList a ls) = AnnList a (orderDefs ls)


-- | Transform a list of definitions where the defined names are in scope for subsequent definitions
trfScopedSequence :: HsHasName d => (d -> Trf e) -> [d] -> Trf [e]
trfScopedSequence f (def:rest) = (:) <$> f def <*> addToScope def (trfScopedSequence f rest)
trfScopedSequence f [] = pure []

                