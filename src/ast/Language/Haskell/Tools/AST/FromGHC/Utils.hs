-- | Utility functions for transforming the GHC AST representation into our own.
module Language.Haskell.Tools.AST.FromGHC.Utils where

import Control.Monad.Reader
import Data.Maybe
import Data.Function
import Data.List
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.SourceMap
import Language.Haskell.Tools.AST.FromGHC.OrdSrcSpan
import ApiAnnotation
import SrcLoc
import HsSyn
import Outputable

-- | RangeInfo (RI) is an alias for SrcSpan
type RI = SrcSpan

class Annot annot where
  createAnnot :: RI -> annot
  extractRange :: annot -> RI
  
instance Annot SrcSpan where
  createAnnot = id
  extractRange = id


-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: Annot i => (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLoc = trfLocCorrect pure

trfMaybe :: Annot i => (Located a -> Trf (Ann b i)) -> Maybe (Located a) -> Trf (AnnMaybe b i)
trfMaybe f = maybe (pure annNothing) (fmap annJust . f)

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: Annot i => (RI -> Trf RI) -> (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLocCorrect locF f (L l e) = do loc <- locF l
                                  Ann (createAnnot loc) <$> local (\s -> s { contRange = loc }) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: Annot i => (a -> Trf (Maybe (b i))) -> Located a -> Trf (Maybe (Ann b i))
trfMaybeLoc f (L l e) = fmap (Ann (createAnnot l)) <$> local (\s -> s { contRange = l }) (f e)  

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfListLoc :: Annot i => (a -> Trf [b i]) -> Located a -> Trf [Ann b i]
trfListLoc f (L l e) = fmap (Ann (createAnnot l)) <$> local (\s -> s { contRange = l }) (f e)  

annLoc :: Annot a => Trf RI -> Trf (b a) -> Trf (Ann b a)
annLoc locm nodem = do loc <- locm
                       node <- local (\s -> s { contRange = loc }) nodem
                       return (Ann (createAnnot loc) node)

-- | Searches for a token inside the parent element and retrieves its location
tokenLoc :: AnnKeywordId -> Trf RI
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

tokenLocBack :: AnnKeywordId -> Trf RI
tokenLocBack keyw = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> asks contRange <*> asks srcMap)

-- | Searches for tokens in the given order inside the parent element and returns their combined location
tokensLoc :: [AnnKeywordId] -> Trf RI
tokensLoc keys = asks contRange >>= tokensLoc' keys
  where tokensLoc' :: [AnnKeywordId] -> RI -> Trf RI
        tokensLoc' (keyw:rest) r 
          = do spanFirst <- tokenLoc keyw
               spanRest <- tokensLoc' rest (mkSrcSpan (srcSpanEnd spanFirst) (srcSpanEnd r))
               return (combineSrcSpans spanFirst spanRest)                   
        tokensLoc' [] r = pure noSrcSpan
        
-- | Searches for a token and retrieves its location anywhere
uniqueTokenAnywhere :: AnnKeywordId -> Trf RI
uniqueTokenAnywhere keyw = fromMaybe noSrcSpan <$> (getKeywordAnywhere keyw <$> asks srcMap)
        
annCont :: Annot a => Trf (e a) -> Trf (Ann e a)
annCont = annLoc (asks contRange)

copyAnnot :: (Ann a i -> b i) -> Trf (Ann a i) -> Trf (Ann b i)
copyAnnot f at = (\(Ann i a) -> Ann i (f (Ann i a))) <$> at

foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

collectLocs :: [Located e] -> RI
collectLocs = foldLocs . map getLoc

orderDefs :: Annot i => [Ann e i] -> [Ann e i]
orderDefs = sortBy (compare `on` ordSrcSpan . extractRange . _annotation)

advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
advanceAllSrcLoc oth _ = oth
  
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                