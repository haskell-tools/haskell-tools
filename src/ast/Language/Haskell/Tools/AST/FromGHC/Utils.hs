module Language.Haskell.Tools.AST.FromGHC.Utils where

import Control.Monad.Reader
import Data.Maybe
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.SourceMap
import ApiAnnotation
import SrcLoc
import HsSyn

type RI = SrcSpan

trfLoc :: (a -> Trf (b RI)) -> Located a -> Trf (Ann b RI)
trfLoc f (L l e) = Ann l <$> local (\s -> s { contRange = l }) (f e) 

trfLocCorrect :: (RI -> Trf RI) -> (a -> Trf (b RI)) -> Located a -> Trf (Ann b RI)
trfLocCorrect locF f (L l e) = do loc <- locF l
                                  Ann loc <$> local (\s -> s { contRange = loc }) (f e)

trfMaybeLoc :: (a -> Trf (Maybe (b RI))) -> Located a -> Trf (Maybe (Ann b RI))
trfMaybeLoc f (L l e) = fmap (Ann l) <$> local (\s -> s { contRange = l }) (f e)  

trfListLoc :: (a -> Trf [b RI]) -> Located a -> Trf [Ann b RI]
trfListLoc f (L l e) = fmap (Ann l) <$> local (\s -> s { contRange = l }) (f e)  

-- | Searches for a token inside the parent element and retrieves its location
tokenLoc :: AnnKeywordId -> Trf RI
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

-- | Searches for tokens inside the parent element and returns their combined location
tokensLoc :: [AnnKeywordId] -> Trf RI
tokensLoc keys = asks contRange >>= tokensLoc' keys
  where tokensLoc' :: [AnnKeywordId] -> RI -> Trf RI
        tokensLoc' (keyw:rest) r 
          = do spanFirst <- fromMaybe noSrcSpan <$> getKeywordInside keyw r <$> asks srcMap
               spanRest <- tokensLoc' rest (mkSrcSpan (srcSpanEnd spanFirst) (srcSpanEnd r))
               return (combineSrcSpans spanFirst spanRest)
                                       
        tokensLoc' [] r = pure r
        
-- | Searches for a token and retrieves its location anywhere
uniqueTokenAnywhere :: AnnKeywordId -> Trf RI
uniqueTokenAnywhere keyw = fromMaybe noSrcSpan <$> (getKeywordAnywhere keyw <$> asks srcMap)
        
noAnn = Ann noSrcSpan
