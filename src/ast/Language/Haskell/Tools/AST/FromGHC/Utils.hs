-- | Utility functions for transforming the GHC AST representation into our own.
{-# LANGUAGE TypeSynonymInstances 
           , FlexibleInstances
           , LambdaCase
           , ViewPatterns
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
import Control.Lens hiding (element)
import Data.Maybe
import Data.IORef
import Data.Function
import Data.List
import Language.Haskell.Tools.AST.Lenses
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.Modules as AST
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.SourceMap
import Language.Haskell.Tools.AST.FromGHC.OrdSrcSpan

class RangeAnnot annot where
  toNodeAnnot :: SrcSpan -> annot
  toListAnnot :: SrcLoc -> annot
  toOptAnnot :: SrcLoc -> annot
  addSemanticInfo :: SemanticInfo -> annot -> annot
  extractRange :: annot -> SrcSpan
  addImportData :: Ann AST.ImportDecl annot -> Trf (Ann AST.ImportDecl annot)
  
instance RangeAnnot RangeWithName where
  toNodeAnnot = NodeInfo NoSemanticInfo . NodeSpan
  toListAnnot = NodeInfo NoSemanticInfo . ListPos
  toOptAnnot = NodeInfo NoSemanticInfo . OptionalPos
  addSemanticInfo si ni = ni & semanticInfo .~ si
  extractRange = spanRange . view sourceInfo
  addImportData = addImportData'

contRangeAnnot :: RangeAnnot a => Trf a
contRangeAnnot = toNodeAnnot <$> asks contRange

addImportData' :: Ann AST.ImportDecl RangeWithName -> Trf (Ann AST.ImportDecl RangeWithName)
addImportData' imp = lift $ 
  do eps <- getSession >>= liftIO . readIORef . hsc_EPS
     mod <- findModule (mkModuleName . nameString $ imp ^. element.importModule.element) 
                       (fmap mkFastString $ imp ^? element.importPkg.annMaybe._Just.element.stringNodeStr)
     let importedNames = concatMap availNames $ maybe [] mi_exports 
                                              $ flip lookupModuleEnv mod 
                                              $ eps_PIT eps
     names <- filterM (checkImportVisible (imp ^. element)) importedNames
     return $ (imp & annotation %~ addSemanticInfo (ImportInfo mod importedNames names))
       
checkImportVisible :: GhcMonad m => AST.ImportDecl RangeWithName -> GHC.Name -> m Bool
checkImportVisible imp name
  | importIsExact imp 
  = or <$> mapM (`ieSpecMatches` name) (imp ^.. importExacts)
  | importIsHiding imp 
  = not . or <$> mapM (`ieSpecMatches` name) (imp ^.. importHidings)
  | otherwise = return True

ieSpecMatches :: GhcMonad m => AST.IESpec RangeWithName -> GHC.Name -> m Bool
ieSpecMatches (AST.IESpec ((^?! annotation.semanticInfo.nameInfo) -> n) ss) name
  | n == name = return True
  | isTyConName n
  = (\case Just (ATyCon tc) -> name `elem` map getName (tyConDataCons tc)) 
             <$> lookupGlobalName n
  | otherwise = return False
  
    
instance RangeAnnot RangeInfo where
  toNodeAnnot = NodeInfo () . NodeSpan
  toListAnnot = NodeInfo () . ListPos
  toOptAnnot = NodeInfo () . OptionalPos
  addSemanticInfo si = id
  extractRange = spanRange . view sourceInfo
  addImportData = pure


nothing pos = annNothing . toOptAnnot <$> pos 
makeList ann ls = AnnList <$> (toListAnnot <$> ann) <*> ls
  
-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: RangeAnnot i => (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLoc = trfLocCorrect pure

trfMaybe :: RangeAnnot i => (Located a -> Trf (Ann e i)) -> Maybe (Located a) -> Trf (AnnMaybe e i)
trfMaybe f (Just e) = annJust <$> f e
trfMaybe f Nothing = annNothing . toNodeAnnot <$> asks contRange

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: RangeAnnot i => (SrcSpan -> Trf SrcSpan) -> (a -> Trf (b i)) -> Located a -> Trf (Ann b i)
trfLocCorrect locF f (L l e) = do loc <- locF l
                                  Ann (toNodeAnnot loc) <$> local (\s -> s { contRange = loc }) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: RangeAnnot i => (a -> Trf (Maybe (b i))) -> Located a -> Trf (Maybe (Ann b i))
trfMaybeLoc f (L l e) = fmap (Ann (toNodeAnnot l)) <$> local (\s -> s { contRange = l }) (f e)  

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfListLoc :: RangeAnnot i => (a -> Trf [b i]) -> Located a -> Trf [Ann b i]
trfListLoc f (L l e) = fmap (Ann (toNodeAnnot l)) <$> local (\s -> s { contRange = l }) (f e)

trfAnnList :: RangeAnnot i => (a -> Trf (b i)) -> [Located a] -> Trf (AnnList b i)
trfAnnList _ [] = AnnList <$> contRangeAnnot <*> pure []
trfAnnList f ls = AnnList <$> pure (toNodeAnnot $ collectLocs ls) <*> mapM (trfLoc f) ls

nonemptyAnnList :: RangeAnnot i => [Ann e i] -> AnnList e i
nonemptyAnnList = AnnList (toNodeAnnot noSrcSpan)

annLoc :: RangeAnnot a => Trf SrcSpan -> Trf (b a) -> Trf (Ann b a)
annLoc locm nodem = do loc <- locm
                       node <- local (\s -> s { contRange = loc }) nodem
                       return (Ann (toNodeAnnot loc) node)

-- | Focuses the transformation to go between tokens
between :: AnnKeywordId -> AnnKeywordId -> Trf a -> Trf a
between firstTok lastTok trf
  = do firstToken <- tokenLoc firstTok
       lastToken <- tokenLoc lastTok
       local (\s -> s { contRange = mkSrcSpan (srcSpanEnd firstToken) (srcSpanStart lastToken)}) trf
       
-- | Gets the position before the given token
before :: AnnKeywordId -> Trf SrcLoc
before tok = srcSpanStart <$> tokenLoc tok
               
-- | Gets the position after the given token
after :: AnnKeywordId -> Trf SrcLoc
after tok = srcSpanEnd <$> tokenLoc tok

-- | Gets the position at the beginning of the focus       
atTheStart :: Trf SrcLoc
atTheStart = asks (srcSpanStart . contRange)
            
-- | Gets the position at the end of the focus      
atTheEnd :: Trf SrcLoc
atTheEnd = asks (srcSpanEnd . contRange)
                 
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
orderAnnList (AnnList a ls) = AnnList a (orderDefs ls)

advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
advanceAllSrcLoc oth _ = oth
  
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                