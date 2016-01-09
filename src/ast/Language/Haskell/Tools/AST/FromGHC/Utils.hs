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

trfMaybeLoc :: (a -> Trf (Maybe (b RI))) -> Located a -> Trf (Maybe (Ann b RI))
trfMaybeLoc f (L l e) = fmap (Ann l) <$> local (\s -> s { contRange = l }) (f e)  

trfListLoc :: (a -> Trf [b RI]) -> Located a -> Trf [Ann b RI]
trfListLoc f (L l e) = fmap (Ann l) <$> local (\s -> s { contRange = l }) (f e)  


tokenLoc :: AnnKeywordId -> Trf RI
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

tokensLoc :: [AnnKeywordId] -> Trf RI
tokensLoc keys = asks contRange >>= tokensLoc' keys
  where tokensLoc' :: [AnnKeywordId] -> RI -> Trf RI
        tokensLoc' (keyw:rest) r 
          = do spanFirst <- fromMaybe noSrcSpan <$> getKeywordInside keyw r <$> asks srcMap
               spanRest <- tokensLoc' rest (mkSrcSpan (srcSpanEnd spanFirst) (srcSpanEnd r))
               return (combineSrcSpans spanFirst spanRest)
                                       
        tokensLoc' [] r = pure r

noAnn = Ann noSrcSpan
