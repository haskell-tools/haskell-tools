-- | Utility functions for transforming the GHC AST representation into our own.
{-# LANGUAGE TypeSynonymInstances 
           , FlexibleInstances
           , LambdaCase
           , ViewPatterns
           , MultiParamTypeClasses
           , FlexibleContexts
           , AllowAmbiguousTypes
           , TypeApplications
           , TypeFamilies
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
import NameSet
import Outputable
import FastString

import Control.Monad.Reader
import Control.Reference hiding (element)
import Data.Maybe
import Data.IORef
import Data.Function hiding ((&))
import Data.List
import Data.Char
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Debug.Trace

-- | Creates a semantic information for a name
createNameInfo :: n -> Trf (NameInfo n)
createNameInfo name = do locals <- asks localsInScope
                         isDefining <- asks defining
                         return (NameInfo locals isDefining name)


-- | Creates a semantic information for an ambiguous name (caused by field disambiguation for example)
createAmbigousNameInfo :: RdrName -> SrcSpan -> Trf (NameInfo n)
createAmbigousNameInfo name span = do locals <- asks localsInScope
                                      isDefining <- asks defining
                                      return (AmbiguousNameInfo locals isDefining name span)

-- | Creates a semantic information for an implicit name
createImplicitNameInfo :: String -> Trf (NameInfo n)
createImplicitNameInfo name = do locals <- asks localsInScope
                                 isDefining <- asks defining
                                 rng <- asks contRange
                                 return (ImplicitNameInfo locals isDefining name rng)

-- | Creates a semantic information for an implicit name
createImplicitFldInfo :: (GHCName n, HsHasName n) => (a -> n) -> [HsRecField n a] -> Trf ImplicitFieldInfo
createImplicitFldInfo select flds = return (ImplicitFieldInfo (map getLabelAndExpr flds))
  where getLabelAndExpr fld = ( head $ hsGetNames $ unLoc (getFieldOccName (hsRecFieldLbl fld))
                              , head $ hsGetNames $ select (hsRecFieldArg fld) )

-- | Adds semantic information to an impord declaration. See ImportInfo.
createImportData :: (HsHasName n, GHCName n) => AST.ImportDecl (Dom n) stage -> Trf (ImportInfo n)
createImportData imp = 
  do (mod,importedNames) <- getImportedNames (imp ^. importModule&element&AST.moduleNameString)
                                             (imp ^? importPkg&annJust&element&stringNodeStr)
     names <- liftGhc $ filterM (checkImportVisible imp) importedNames
     lookedUpNames <- liftGhc $ mapM (getFromNameUsing getTopLevelId) names
     lookedUpImported <- liftGhc $ mapM (getFromNameUsing getTopLevelId) importedNames
     return $ ImportInfo mod (catMaybes lookedUpImported) (catMaybes lookedUpNames)

-- | Get names that are imported from a given import
getImportedNames :: String -> Maybe String -> Trf (GHC.Module, [GHC.Name])
getImportedNames name pkg = liftGhc $ do
  eps <- getSession >>= liftIO . readIORef . hsc_EPS
  mod <- findModule (mkModuleName name) (fmap mkFastString pkg)
  -- load exported names from interface file
  let ifaceNames = concatMap availNames $ maybe [] mi_exports 
                                        $ flip lookupModuleEnv mod 
                                        $ eps_PIT eps
  loadedNames <- maybe [] modInfoExports <$> getModuleInfo mod
  return (mod, ifaceNames ++ loadedNames)

-- | Check is a given name is imported from an import with given import specification.
checkImportVisible :: (HsHasName n, GhcMonad m) => AST.ImportDecl (Dom n) stage -> GHC.Name -> m Bool
checkImportVisible imp name
  | importIsExact imp 
  = or @[] <$> mapM (`ieSpecMatches` name) (imp ^? importExacts)
  | importIsHiding imp 
  = not . or  @[] <$> mapM (`ieSpecMatches` name) (imp ^? importHidings)
  | otherwise = return True

ieSpecMatches :: (HsHasName n, GhcMonad m) => AST.IESpec (Dom n) stage -> GHC.Name -> m Bool
ieSpecMatches (AST.IESpec (hsGetNames <=< (^? element&simpleName&semantics&nameInfo) -> [n]) ss) name
  | n == name = return True
  | isTyConName n
  = (\case Just (ATyCon tc) -> name `elem` map getName (tyConDataCons tc)) 
             <$> lookupName n
ieSpecMatches _ _ = return False

noSemaInfo :: src -> NodeInfo NoSemanticInfo src
noSemaInfo = NodeInfo NoSemanticInfo

-- | Creates a place for a missing node with a default location
nothing :: String -> String -> Trf SrcLoc -> Trf (AnnMaybe e (Dom n) RangeStage)
nothing bef aft pos = annNothing . noSemaInfo . OptionalPos bef aft <$> pos 

emptyList :: String -> Trf SrcLoc -> Trf (AnnList e (Dom n) RangeStage)
emptyList sep ann = AnnList <$> (noSemaInfo . ListPos "" "" sep False <$> ann) <*> pure []

-- | Creates a place for a list of nodes with a default place if the list is empty.
makeList :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeList sep ann ls = AnnList <$> (noSemaInfo . ListPos "" "" sep False <$> ann) <*> ls

makeListBefore :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeListBefore bef sep ann ls = do isEmpty <- null <$> ls 
                                   AnnList <$> (noSemaInfo . ListPos (if isEmpty then bef else "") "" sep False <$> ann) <*> ls

makeListAfter :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeListAfter aft sep ann ls = do isEmpty <- null <$> ls 
                                  AnnList <$> (noSemaInfo . ListPos "" (if isEmpty then aft else "") sep False <$> ann) <*> ls

makeNonemptyList :: String -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeNonemptyList sep ls = AnnList (noSemaInfo $ ListPos "" "" sep False noSrcLoc) <$> ls

-- | Creates a place for an indented list of nodes with a default place if the list is empty.
makeIndentedList :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeIndentedList ann ls = AnnList <$> (noSemaInfo . ListPos  "" "" "\n" True <$> ann) <*> ls

makeIndentedListNewlineBefore :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeIndentedListNewlineBefore ann ls = do isEmpty <- null <$> ls 
                                          AnnList <$> (noSemaInfo . ListPos (if isEmpty then "\n" else "") "" "\n" True <$> ann) <*> ls

makeIndentedListBefore :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeIndentedListBefore bef sp ls = do isEmpty <- null <$> ls 
                                      AnnList <$> (noSemaInfo . ListPos (if isEmpty then bef else "") "" "\n" True <$> sp) <*> ls
  
makeNonemptyIndentedList :: Trf [Ann e (Dom n) RangeStage] -> Trf (AnnList e (Dom n) RangeStage)
makeNonemptyIndentedList ls = AnnList (noSemaInfo $ ListPos "" "" "\n" True noSrcLoc) <$> ls
  
-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: (a -> Trf (b (Dom n) RangeStage)) -> Trf (SemanticInfo (Dom n) b) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLoc f sema = trfLocCorrect sema pure f

trfLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => (a -> Trf (b (Dom n) RangeStage)) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLocNoSema f = trfLoc f (pure NoSemanticInfo)

-- | Transforms a possibly-missing node with the default location of the end of the focus.
trfMaybe :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Maybe (Located a) -> Trf (AnnMaybe e (Dom n) RangeStage)
trfMaybe bef aft f = trfMaybeDefault bef aft f atTheEnd

-- | Transforms a possibly-missing node with a default location
trfMaybeDefault :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Trf SrcLoc -> Maybe (Located a) -> Trf (AnnMaybe e (Dom n) RangeStage)
trfMaybeDefault _   _   f _   (Just e) = makeJust <$> f e
trfMaybeDefault bef aft _ loc Nothing  = nothing bef aft loc

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: Trf (SemanticInfo (Dom n) b) -> (SrcSpan -> Trf SrcSpan) -> (a -> Trf (b (Dom n) RangeStage)) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLocCorrect sema locF f (L l e) = annLoc sema (locF l) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: (a -> Trf (Maybe (b (Dom n) RangeStage))) -> SemanticInfo (Dom n) b -> Located a -> Trf (Maybe (Ann b (Dom n) RangeStage))
trfMaybeLoc f sema (L l e) = do fmap (Ann (NodeInfo sema (NodeSpan l))) <$> local (\s -> s { contRange = l }) (f e)

trfMaybeLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => (a -> Trf (Maybe (b (Dom n) RangeStage))) -> Located a -> Trf (Maybe (Ann b (Dom n) RangeStage))
trfMaybeLocNoSema f = trfMaybeLoc f NoSemanticInfo

-- | Creates a place for a list of nodes with the default place at the end of the focus if the list is empty.
trfAnnList ::SemanticInfo (Dom n) b ~ NoSemanticInfo =>  String -> (a -> Trf (b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnList b (Dom n) RangeStage)
trfAnnList sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList sep f ls = makeList sep (pure $ noSrcLoc) (mapM (trfLoc f (pure NoSemanticInfo)) ls)

trfAnnList' :: String -> (Located a -> Trf (Ann b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnList b (Dom n) RangeStage)
trfAnnList' sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList' sep f ls = makeList sep (pure $ noSrcLoc) (mapM f ls)


-- | Creates a place for a list of nodes that cannot be empty.
nonemptyAnnList :: [Ann e (Dom n) RangeStage] -> AnnList e (Dom n) RangeStage
nonemptyAnnList = AnnList (noSemaInfo $ ListPos "" "" "" False noSrcLoc)

-- | Creates an optional node from an existing element
makeJust :: Ann e (Dom n) RangeStage -> AnnMaybe e (Dom n) RangeStage
makeJust e = AnnMaybe (noSemaInfo $ OptionalPos "" "" noSrcLoc) (Just e)

-- | Annotates a node with the given location and focuses on the given source span.
annLoc :: Trf (SemanticInfo (Dom n) b) -> Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLoc semam locm nodem = do loc <- locm
                             node <- focusOn loc nodem
                             sema <- semam
                             return (Ann (NodeInfo sema (NodeSpan loc)) node)

annLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLocNoSema = annLoc (pure NoSemanticInfo)

-- * Focus manipulation

focusOn :: SrcSpan -> Trf a -> Trf a
focusOn sp = local (\s -> s { contRange = sp })

updateFocus :: (SrcSpan -> Trf SrcSpan) -> Trf a -> Trf a
updateFocus f trf = do newSpan <- f =<< asks contRange
                       focusOn newSpan trf

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

-- | Focuses the transformation to be performed before the given token. The token must be found inside the current range.
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
annFrom :: AnnKeywordId -> Trf (SemanticInfo (Dom n) e) -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annFrom kw sema = annLoc sema (combineSrcSpans <$> tokenLoc kw <*> asks (srcLocSpan . srcSpanEnd . contRange))

annFromNoSema :: SemanticInfo (Dom n) e ~ NoSemanticInfo => AnnKeywordId -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annFromNoSema kw = annFrom kw (pure NoSemanticInfo)

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

allTokensAfter :: SrcLoc -> Trf [(SrcSpan, AnnKeywordId)]
allTokensAfter loc = getTokensAfter loc <$> asks srcMap

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
annCont :: Trf (SemanticInfo (Dom n) e) -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annCont sema = annLoc sema (asks contRange)

annContNoSema :: SemanticInfo (Dom n) e ~ NoSemanticInfo => Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annContNoSema = annCont (pure NoSemanticInfo)

-- | Annotates the element with the same annotation that is on the other element
copyAnnot :: SemanticInfo (Dom n) a ~ SemanticInfo (Dom n) b 
               => (Ann a (Dom n) RangeStage -> b (Dom n) RangeStage) -> Trf (Ann a (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
copyAnnot f at = (\(Ann i a) -> Ann i (f (Ann i a))) <$> at

-- | Combine source spans into one that contains them all
foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

-- | The location after the given string
advanceStr :: String -> SrcLoc -> SrcLoc
advanceStr str (RealSrcLoc l) = RealSrcLoc $ foldl advanceSrcLoc l str
advanceStr _ l = l

-- | Update column information in a source location
updateCol :: (Int -> Int) -> SrcLoc -> SrcLoc
updateCol f loc@(UnhelpfulLoc _) = loc
updateCol f (RealSrcLoc loc) = mkSrcLoc (srcLocFile loc) (srcLocLine loc) (f $ srcLocCol loc)

-- | Update the start of the src span
updateStart :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateStart f sp = mkSrcSpan (f (srcSpanStart sp)) (srcSpanEnd sp)

-- | Update the end of the src span
updateEnd :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateEnd f sp = mkSrcSpan (srcSpanStart sp) (f (srcSpanEnd sp))

-- | Combine source spans of elements into one that contains them all
collectLocs :: [Located e] -> SrcSpan
collectLocs = foldLocs . map getLoc

-- | Rearrange definitions to appear in the order they are defined in the source file.
orderDefs :: [Ann e (Dom n) RangeStage] -> [Ann e (Dom n) RangeStage]
orderDefs = sortBy (compare `on` AST.ordSrcSpan . (^. AST.annotation & AST.sourceInfo & AST.nodeSpan))

-- | Orders a list of elements to the order they are defined in the source file.
orderAnnList :: AnnList e (Dom n) RangeStage -> AnnList e (Dom n) RangeStage
orderAnnList (AnnList a ls) = AnnList a (orderDefs ls)


-- | Transform a list of definitions where the defined names are in scope for subsequent definitions
trfScopedSequence :: HsHasName d => (d -> Trf e) -> [d] -> Trf [e]
trfScopedSequence f (def:rest) = (:) <$> f def <*> addToScope def (trfScopedSequence f rest)
trfScopedSequence f [] = pure []

-- | Splits a given string at whitespaces while calculating the source location of the fragments
splitLocated :: Located String -> [Located String]
splitLocated (L (RealSrcSpan l) str) = splitLocated' str (realSrcSpanStart l) Nothing
  where splitLocated' :: String -> RealSrcLoc -> Maybe (RealSrcLoc, String) -> [Located String]
        splitLocated' (c:rest) currLoc (Just (startLoc, str)) | isSpace c 
          = L (RealSrcSpan $ mkRealSrcSpan startLoc currLoc) (reverse str) : splitLocated' rest (advanceSrcLoc currLoc c) Nothing
        splitLocated' (c:rest) currLoc Nothing | isSpace c = splitLocated' rest (advanceSrcLoc currLoc c) Nothing
        splitLocated' (c:rest) currLoc (Just (startLoc, str)) = splitLocated' rest (advanceSrcLoc currLoc c) (Just (startLoc, c:str))
        splitLocated' (c:rest) currLoc Nothing = splitLocated' rest (advanceSrcLoc currLoc c) (Just (currLoc, [c]))
        splitLocated' [] currLoc (Just (startLoc, str)) = [L (RealSrcSpan $ mkRealSrcSpan startLoc currLoc) (reverse str)]
        splitLocated' [] currLoc Nothing = []
                