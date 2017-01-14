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

import ApiAnnotation (AnnKeywordId(..))
import Avail (availNamesWithSelectors, availNames)
import BasicTypes (StringLiteral(..))
import CoAxiom as GHC (CoAxiom(..))
import CoreSyn as GHC (isOrphan)
import DynFlags (xopt)
import FamInstEnv as GHC (FamInst(..), famInstEnvElts)
import FastString (unpackFS, mkFastString)
import FieldLabel as GHC (FieldLbl(..))
import GHC
import HsSyn
import HscTypes
import Id (idName)
import InstEnv as GHC (ClsInst(..), instanceDFunId, instEnvElts)
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Module as GHC
import Name
import SrcLoc

import Control.Monad.Reader
import Control.Reference ((^.), (&))
import Data.Char (isSpace)
import Data.Either (Either(..), rights, lefts)
import Data.Function hiding ((&))
import Data.IORef (readIORef)
import Data.List
import Data.Maybe
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.SourceMap
import Language.Haskell.Tools.AST.SemaInfoTypes as Sema

createModuleInfo :: ModSummary -> Trf (Sema.ModuleInfo GHC.Name)
createModuleInfo mod = do 
  let prelude = xopt ImplicitPrelude $ ms_hspp_opts mod
  (_,preludeImports) <- if prelude then getImportedNames "Prelude" Nothing else return (ms_mod mod, [])
  (insts, famInsts) <- if prelude then lift $ getOrphanAndFamInstances (Module baseUnitId (GHC.mkModuleName "Prelude")) 
                                  else return ([], [])
  return $ mkModuleInfo (ms_mod mod) (case ms_hsc_src mod of HsSrcFile -> False; _ -> True) preludeImports insts famInsts

-- | Creates a semantic information for a name
createNameInfo :: n -> Trf (NameInfo n)
createNameInfo name = do locals <- asks localsInScope
                         isDefining <- asks defining
                         return (mkNameInfo locals isDefining name)


-- | Creates a semantic information for an ambiguous name (caused by field disambiguation for example)
createAmbigousNameInfo :: RdrName -> SrcSpan -> Trf (NameInfo n)
createAmbigousNameInfo name span = do locals <- asks localsInScope
                                      isDefining <- asks defining
                                      return (mkAmbiguousNameInfo locals isDefining name span)

-- | Creates a semantic information for an implicit name
createImplicitNameInfo :: String -> Trf (NameInfo n)
createImplicitNameInfo name = do locals <- asks localsInScope
                                 isDefining <- asks defining
                                 rng <- asks contRange
                                 return (mkImplicitNameInfo locals isDefining name rng)

-- | Creates a semantic information for an implicit name
createImplicitFldInfo :: (GHCName n, HsHasName n) => (a -> n) -> [HsRecField n a] -> Trf ImplicitFieldInfo
createImplicitFldInfo select flds = return (mkImplicitFieldInfo (map getLabelAndExpr flds))
  where getLabelAndExpr fld = ( head $ hsGetNames $ unLoc (getFieldOccName (hsRecFieldLbl fld))
                              , head $ hsGetNames $ select (hsRecFieldArg fld) )

-- | Adds semantic information to an impord declaration. See ImportInfo.
createImportData :: (GHCName r, HsHasName n) => GHC.ImportDecl n -> Trf (ImportInfo r)
createImportData (GHC.ImportDecl _ name pkg _ _ _ _ _ declHiding) = 
  do (mod,importedNames) <- getImportedNames (GHC.moduleNameString $ unLoc name) (fmap (unpackFS . sl_fs) pkg)
     names <- liftGhc $ filterM (checkImportVisible declHiding) importedNames
     lookedUpNames <- liftGhc $ mapM (getFromNameUsing getTopLevelId) names
     lookedUpImported <- liftGhc $ mapM (getFromNameUsing getTopLevelId) importedNames
     (insts,famInsts) <- lift $ getOrphanAndFamInstances mod
     return $ mkImportInfo mod (catMaybes lookedUpImported) (catMaybes lookedUpNames) insts famInsts

getOrphanAndFamInstances :: Module -> Ghc ([ClsInst], [FamInst])
getOrphanAndFamInstances mod = do      
  env <- getSession
  eps <- liftIO (readIORef (hsc_EPS env))
  let ifc = lookupIfaceByModule (hsc_dflags env) (hsc_HPT env) (eps_PIT eps) mod
      hp = lookupHptByModule (hsc_HPT env) mod
      uses = catMaybes $ map getModule $ maybe [] (\ifc -> dep_orphs (mi_deps ifc) `union` dep_finsts (mi_deps ifc)) ifc
      getModule mod = if moduleUnitId mod == mainUnitId 
                        then fmap Right $ lookupHptByModule (hsc_HPT env) mod
                        else fmap Left $ lookupIfaceByModule (hsc_dflags env) (hsc_HPT env) (eps_PIT eps) mod
      usedMods = lefts uses
      usedDetails = map hm_details (maybeToList hp ++ rights uses)
      hptInstances = filter (isOrphan . is_orphan) $ concatMap md_insts usedDetails
      hptFamilyInstances = concatMap md_fam_insts usedDetails
      allInstances = instEnvElts (eps_inst_env eps)
      relevantInstances = hptInstances ++ filter ((\n -> nameModule n `elem` (mod : map mi_module usedMods)) . idName . instanceDFunId) (filter (isOrphan . is_orphan) allInstances)
      allFamilyInstances = famInstEnvElts (eps_fam_inst_env eps)
      relevantFamilyInstances = hptFamilyInstances ++ filter ((\n -> nameModule n `elem` (mod : map mi_module usedMods)) . co_ax_name . fi_axiom) allFamilyInstances
  return (relevantInstances, relevantFamilyInstances)


-- | Get names that are imported from a given import
getImportedNames :: String -> Maybe String -> Trf (GHC.Module, [GHC.Name])
getImportedNames name pkg = liftGhc $ do
  hpt <- hsc_HPT <$> getSession
  eps <- getSession >>= liftIO . readIORef . hsc_EPS
  mod <- findModule (mkModuleName name) (fmap mkFastString pkg)
  -- load exported names from interface file
  let ifaceNames = concatMap availNames $ maybe [] mi_exports 
                                        $ flip lookupModuleEnv mod 
                                        $ eps_PIT eps
  let homeExports = maybe [] (md_exports . hm_details) (lookupHptByModule hpt mod)
  mi <- getModuleInfo mod
  return (mod, ifaceNames ++ concatMap availNamesWithSelectors homeExports)

-- | Check is a given name is imported from an import with given import specification.
checkImportVisible :: (HsHasName name, GhcMonad m) => Maybe (Bool, Located [LIE name]) -> GHC.Name -> m Bool
checkImportVisible (Just (isHiding, specs)) name
  | isHiding  = not . or @[] <$> mapM (`ieSpecMatches` name) (map unLoc (unLoc specs))
  | otherwise = or @[] <$> mapM (`ieSpecMatches` name) (map unLoc (unLoc specs))
checkImportVisible _ _ = return True

ieSpecMatches :: (HsHasName name, GhcMonad m) => IE name -> GHC.Name -> m Bool
ieSpecMatches (hsGetNames . HsSyn.ieName -> [n]) name
  | n == name = return True
  | isTyConName n
  = do entity <- lookupName n
       return $ case entity of Just (ATyCon tc) 
                                 | Just cls <- tyConClass_maybe tc 
                                     -> name `elem` map getName (classMethods cls)
                                 | otherwise -> name `elem` concatMap (\dc -> getName dc : map flSelector (dataConFieldLabels dc)) 
                                                                      (tyConDataCons tc)
                               _             -> False
ieSpecMatches _ _ = return False

noSemaInfo :: src -> NodeInfo NoSemanticInfo src
noSemaInfo = NodeInfo mkNoSemanticInfo
 
-- | Creates a place for a missing node with a default location
nothing :: String -> String -> Trf SrcLoc -> Trf (AnnMaybeG e (Dom n) RangeStage)
nothing bef aft pos = annNothing . noSemaInfo . OptionalPos bef aft <$> pos 

emptyList :: String -> Trf SrcLoc -> Trf (AnnListG e (Dom n) RangeStage)
emptyList sep ann = AnnListG <$> (noSemaInfo . ListPos "" "" sep False <$> ann) <*> pure []

-- | Creates a place for a list of nodes with a default place if the list is empty.
makeList :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeList sep ann ls = AnnListG <$> (noSemaInfo . ListPos "" "" sep False <$> ann) <*> ls

makeListBefore :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeListBefore bef sep ann ls = do isEmpty <- null <$> ls 
                                   AnnListG <$> (noSemaInfo . ListPos (if isEmpty then bef else "") "" sep False <$> ann) <*> ls

makeListAfter :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeListAfter aft sep ann ls = do isEmpty <- null <$> ls 
                                  AnnListG <$> (noSemaInfo . ListPos "" (if isEmpty then aft else "") sep False <$> ann) <*> ls

makeNonemptyList :: String -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeNonemptyList sep ls = AnnListG (noSemaInfo $ ListPos "" "" sep False noSrcLoc) <$> ls

-- | Creates a place for an indented list of nodes with a default place if the list is empty.
makeIndentedList :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedList ann ls = AnnListG <$> (noSemaInfo . ListPos  "" "" "\n" True <$> ann) <*> ls

makeIndentedListNewlineBefore :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedListNewlineBefore ann ls = do isEmpty <- null <$> ls 
                                          AnnListG <$> (noSemaInfo . ListPos (if isEmpty then "\n" else "") "" "\n" True <$> ann) <*> ls

makeIndentedListBefore :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedListBefore bef sp ls = do isEmpty <- null <$> ls 
                                      AnnListG <$> (noSemaInfo . ListPos (if isEmpty then bef else "") "" "\n" True <$> sp) <*> ls
  
makeNonemptyIndentedList :: Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeNonemptyIndentedList ls = AnnListG (noSemaInfo $ ListPos "" "" "\n" True noSrcLoc) <$> ls
  
-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: (a -> Trf (b (Dom n) RangeStage)) -> Trf (SemanticInfo (Dom n) b) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLoc f sema = trfLocCorrect sema pure f

trfLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => (a -> Trf (b (Dom n) RangeStage)) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLocNoSema f = trfLoc f (pure mkNoSemanticInfo)

-- | Transforms a possibly-missing node with the default location of the end of the focus.
trfMaybe :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Maybe (Located a) -> Trf (AnnMaybeG e (Dom n) RangeStage)
trfMaybe bef aft f = trfMaybeDefault bef aft f atTheEnd

-- | Transforms a possibly-missing node with a default location
trfMaybeDefault :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Trf SrcLoc -> Maybe (Located a) -> Trf (AnnMaybeG e (Dom n) RangeStage)
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
trfMaybeLocNoSema f = trfMaybeLoc f mkNoSemanticInfo

-- | Creates a place for a list of nodes with the default place at the end of the focus if the list is empty.
trfAnnList :: SemanticInfo (Dom n) b ~ NoSemanticInfo => String -> (a -> Trf (b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnListG b (Dom n) RangeStage)
trfAnnList sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList sep f ls = makeList sep (pure $ noSrcLoc) (mapM (trfLoc f (pure mkNoSemanticInfo)) ls)

trfAnnList' :: String -> (Located a -> Trf (Ann b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnListG b (Dom n) RangeStage)
trfAnnList' sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList' sep f ls = makeList sep (pure $ noSrcLoc) (mapM f ls)


-- | Creates a place for a list of nodes that cannot be empty.
nonemptyAnnList :: [Ann e (Dom n) RangeStage] -> AnnListG e (Dom n) RangeStage
nonemptyAnnList = AnnListG (noSemaInfo $ ListPos "" "" "" False noSrcLoc)

-- | Creates an optional node from an existing element
makeJust :: Ann e (Dom n) RangeStage -> AnnMaybeG e (Dom n) RangeStage
makeJust e = AnnMaybeG (noSemaInfo $ OptionalPos "" "" noSrcLoc) (Just e)

-- | Annotates a node with the given location and focuses on the given source span.
annLoc :: Trf (SemanticInfo (Dom n) b) -> Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLoc semam locm nodem = do loc <- locm
                             node <- focusOn loc nodem
                             sema <- semam
                             return (Ann (NodeInfo sema (NodeSpan loc)) node)

annLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLocNoSema = annLoc (pure mkNoSemanticInfo)

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
annFromNoSema kw = annFrom kw (pure mkNoSemanticInfo)

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
        tokensLoc' [] _ = pure noSrcSpan
        
-- | Searches for a token and retrieves its location anywhere
uniqueTokenAnywhere :: AnnKeywordId -> Trf SrcSpan
uniqueTokenAnywhere keyw = fromMaybe noSrcSpan <$> (getKeywordAnywhere keyw <$> asks srcMap)
        
-- | Annotates the given element with the current focus as a location.
annCont :: Trf (SemanticInfo (Dom n) e) -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annCont sema = annLoc sema (asks contRange)

annContNoSema :: SemanticInfo (Dom n) e ~ NoSemanticInfo => Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annContNoSema = annCont (pure mkNoSemanticInfo)

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
updateCol _ loc@(UnhelpfulLoc _) = loc
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
orderAnnList :: AnnListG e (Dom n) RangeStage -> AnnListG e (Dom n) RangeStage
orderAnnList (AnnListG a ls) = AnnListG a (orderDefs ls)

-- | Only keeps one of the elements that are on the same source location
removeDuplicates :: [Located e] -> [Located e]
removeDuplicates (fst:rest) = fst : removeDuplicates (filter ((/= getLoc fst) . getLoc) rest)
removeDuplicates [] = []

-- | Transform a list of definitions where the defined names are in scope for subsequent definitions
trfScopedSequence :: HsHasName d => (d -> Trf e) -> [d] -> Trf [e]
trfScopedSequence f (def:rest) = (:) <$> f def <*> addToScope def (trfScopedSequence f rest)
trfScopedSequence _ [] = pure []

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
        splitLocated' [] _ Nothing = []
splitLocated _ = error "splitLocated: unhelpful span given"