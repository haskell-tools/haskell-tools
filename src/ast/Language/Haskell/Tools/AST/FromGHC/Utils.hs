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

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: RangeAnnot i => (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLoc = trfLocCorrect pure

trfMaybe :: RangeAnnot i => (Located a -> Trf (Ann b i)) -> Maybe (Located a) -> Trf (AnnMaybe b i)
trfMaybe f = maybe (pure annNothing) (fmap annJust . f)

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: RangeAnnot i => (SrcSpan -> Trf SrcSpan) -> (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLocCorrect locF f (L l e) = do loc <- locF l
                                  Ann (toRangeAnnot loc) <$> local (\s -> s { contRange = loc }) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: RangeAnnot i => (a -> Trf (Maybe (b i))) -> Located a -> Trf (Maybe (Ann b i))
trfMaybeLoc f (L l e) = fmap (Ann (toRangeAnnot l)) <$> local (\s -> s { contRange = l }) (f e)  

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfListLoc :: RangeAnnot i => (a -> Trf [b i]) -> Located a -> Trf [Ann b i]
trfListLoc f (L l e) = fmap (Ann (toRangeAnnot l)) <$> local (\s -> s { contRange = l }) (f e)  

annLoc :: RangeAnnot a => Trf SrcSpan -> Trf (b a) -> Trf (Ann b a)
annLoc locm nodem = do loc <- locm
                       node <- local (\s -> s { contRange = loc }) nodem
                       return (Ann (toRangeAnnot loc) node)

-- | Searches for a token inside the parent element and retrieves its location
tokenLoc :: AnnKeywordId -> Trf SrcSpan
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

tokenLocBack :: AnnKeywordId -> Trf SrcSpan
tokenLocBack keyw = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> asks contRange <*> asks srcMap)

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
        
annCont :: RangeAnnot a => Trf (e a) -> Trf (Ann e a)
annCont = annLoc (asks contRange)

copyAnnot :: (Ann a i -> b i) -> Trf (Ann a i) -> Trf (Ann b i)
copyAnnot f at = (\(Ann i a) -> Ann i (f (Ann i a))) <$> at

foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

collectLocs :: [Located e] -> SrcSpan
collectLocs = foldLocs . map getLoc

orderDefs :: RangeAnnot i => [Ann e i] -> [Ann e i]
orderDefs = sortBy (compare `on` ordSrcSpan . extractRange . _annotation)

orderAnnList :: RangeAnnot i => AnnList e i -> AnnList e i
orderAnnList (AnnList ls) = AnnList (orderDefs ls)

advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
advanceAllSrcLoc oth _ = oth
  
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                