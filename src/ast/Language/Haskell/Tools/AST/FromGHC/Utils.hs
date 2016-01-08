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
trfMaybeLoc f (L l e) = fmap (Ann l) <$> f e 

tokenLoc :: AnnKeywordId -> Trf RI
tokenLoc keyw = fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

noAnn = Ann noSrcSpan
