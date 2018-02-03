{-# LANGUAGE ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, MonoLocalBinds, MultiWayIf, TypeApplications #-}

-- | Basic utilities and types for defining refactorings.
module Language.Haskell.Tools.Refactor.Utils.Monadic where

import Control.Monad.Reader (Monad(..), ReaderT(..), MonadReader(..))
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.Trans.Except (runExceptT)
import Control.Reference hiding (element)
import Data.Either
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe

import GHC hiding (mkModuleName, moduleNameString)
import qualified Module as GHC (Module(..), moduleNameString)
import qualified Name as GHC
import qualified PrelNames as GHC (basicKnownKeyNames)
import qualified TyCon as GHC (TyCon(..))
import qualified TysWiredIn as GHC (wiredInTyCons)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor.Monad
import Language.Haskell.Tools.Refactor.Representation (RefactorChange(..), ModuleDom, UnnamedModule)
import Language.Haskell.Tools.Rewrite as HT

-- | Performs the given refactoring, transforming it into a Ghc action
runRefactor :: ModuleDom -> [ModuleDom] -> Refactoring -> Ghc (Either String [RefactorChange])
runRefactor mod mods trf = runExceptT $ trf mod mods

-- | Wraps a refactoring that only affects one module. Performs the per-module finishing touches.
localRefactoring :: LocalRefactoring -> Refactoring
localRefactoring ref (name, mod) _
  = (\m -> [ContentChanged (name, m)]) <$> localRefactoringRes id mod (ref mod)

-- | Transform the result of the local refactoring
localRefactoringRes :: ((UnnamedModule -> UnnamedModule) -> a -> a)
                          -> UnnamedModule -> LocalRefactor a -> Refactor a
localRefactoringRes access mod trf
  = let init = RefactorCtx (semanticsModule $ mod ^. semantics) mod (mod ^? modImports&annList)
     in flip runReaderT init $ do (mod, recorded) <- runWriterT (fromRefactorT trf)
                                  return $ access (insertText (rights recorded) . addGeneratedImports (lefts recorded)) mod

-- | Re-inserts the elements removed from the AST that should be kept (for example preprocessor directives)
insertText :: SourceInfoTraversal p => [(SrcSpan,String,String)] -> p dom SrcTemplateStage -> p dom SrcTemplateStage
insertText [] p = p
insertText inserted p
  -- this traverses the AST and finds the positions where the removed elements can be added
  = evalState (sourceInfoTraverseUp (SourceInfoTrf
                (\stn -> sourceTemplateNodeElems !~ takeWhatPrecedesElem (stn ^. sourceTemplateNodeRange) $ stn)
                (srcTmpSeparators !~ takeWhatPrecedesSep)
                pure) (return ()) (return ()) p) (map Right $ sortOn (^. _1) inserted)
  where
   -- insert fragments into list separators
   takeWhatPrecedesSep :: [([SourceTemplateTextElem], SrcSpan)] -> State [Either SrcSpan (SrcSpan,String,String)] [([SourceTemplateTextElem], SrcSpan)]
   takeWhatPrecedesSep seps = takeWhatPrecedes Nothing (Just . (^. _2))
                                               (\str -> _1 .- (++ [StayingText str ""]))
                                               (\str -> _1 .- ([StayingText str ""] ++))
                                               seps

   -- insert fragments into AST elements
   takeWhatPrecedesElem :: SrcSpan -> [SourceTemplateElem] -> State [Either SrcSpan (SrcSpan,String,String)] [SourceTemplateElem]
   takeWhatPrecedesElem rng elems = takeWhatPrecedes (Just rng) (^? sourceTemplateTextRange)
                                                                (\s -> sourceTemplateTextElem .- (++ [StayingText s ""]))
                                                                (\s -> sourceTemplateTextElem .- ([StayingText s ""] ++))
                                                                elems

   -- finds the position of the fragment where there are elements in the template both before and after the fragment
   -- puts holes into the list of inserted fragments where child elements are located
   -- uses these holes to determine where should the fragment be added
   takeWhatPrecedes :: Maybe SrcSpan -> (a -> Maybe SrcSpan) -> (String -> a -> a) -> (String -> a -> a) -> [a] -> State [Either SrcSpan (SrcSpan,String,String)] [a]
   takeWhatPrecedes rng access append prepend elems
     | ranges <- mapMaybe access elems
     , not (null ranges)
     = do let start = srcSpanStart $ fromMaybe (head ranges) rng
              end = srcSpanEnd $ fromMaybe (last ranges) rng
          toInsert <- get
          let (prefix,rest) = break ((>= start) . srcSpanStart . either id (\(sp,_,_) -> sp)) toInsert
              (middle,suffix) = break ((> end) . srcSpanEnd . either id (\(sp,_,_) -> sp)) rest
          put $ prefix ++ Left (mkSrcSpan start end) : suffix
          return $ mergeInserted access append prepend False middle elems
     where mergeInserted :: (a -> Maybe SrcSpan) -> (String -> a -> a) -> (String -> a -> a) -> Bool -> [Either SrcSpan (SrcSpan,String,String)] -> [a] -> [a]
           -- no fragments left
           mergeInserted _ _ _ _ [] elems = elems
           mergeInserted access append prepend prep insert@(Right (insertSpan,insertStr,ln):toInsert) (fstElem:elems)
              -- insert a fragment to the end of the current element if the next elment is after the fragment
              | Just _ <- access fstElem -- TODO: is this needed?
              , not prep && case mapMaybe access elems of sp:_ -> srcSpanStart sp >= srcSpanEnd insertSpan
                                                          _ -> True
              = mergeInserted access append prepend prep toInsert (append (ln ++ insertStr ++ ln) fstElem : elems)
              -- insert the fragment before the current elem if we need an element before (we skipped a child)
              -- and the current element is after the inserted fragment
              | Just fstElemSpace <- access fstElem
              , prep && srcSpanStart fstElemSpace >= srcSpanEnd insertSpan
              = mergeInserted access append prepend prep toInsert (prepend (ln ++ insertStr ++ ln) fstElem : elems)
              | isJust (access fstElem) && prep
              = mergeInserted access append prepend False insert (fstElem : elems) -- switch back to append mode
              | otherwise
              = fstElem : mergeInserted access append prepend (if isJust (access fstElem) then False else prep) insert elems -- move on and switch back to append mode
           -- when found a hole
           mergeInserted access append prepend prep insert@(Left sp : toInsert) (fstElem:elems)
              | Just fstElemSpace <- access fstElem
              = if srcSpanStart fstElemSpace > srcSpanEnd sp
                  -- switch to prepend mode
                  then mergeInserted access append prepend True toInsert (fstElem:elems)
                  -- skip elements that are not after the fragment
                  else fstElem : mergeInserted access append prepend prep insert elems
              | otherwise
              = fstElem : mergeInserted access append prepend True toInsert elems -- switch to prepend mode and move on
           mergeInserted _ _ _ _ _ [] = [] -- maybe error
   takeWhatPrecedes _ _ _ _ elems = return elems

-- | Adds the imports that bring names into scope that are needed by the refactoring
addGeneratedImports :: [GHC.Name] -> HT.Module -> HT.Module
addGeneratedImports names m = modImports&annListElems .- (++ addImports names) $ m
  where addImports :: [GHC.Name] -> [HT.ImportDecl]
        addImports names = map createImport $ groupBy ((==) `on` GHC.nameModule) $ filter (isJust . GHC.nameModule_maybe) $ nub $ sort names

        -- TODO: group names like constructors into correct IESpecs
        createImport :: [GHC.Name] -> HT.ImportDecl
        -- works on groupby result, so list is nonempty
        createImport names = mkImportDecl False True False Nothing (mkModuleName $ GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule $ head names)
                                          Nothing (Just $ mkImportSpecList (map (\n -> mkIESpec (mkUnqualName' n) Nothing) names))

registeredNamesFromPrelude :: [GHC.Name]
registeredNamesFromPrelude = GHC.basicKnownKeyNames ++ map GHC.tyConName GHC.wiredInTyCons

otherNamesFromPrelude :: [String]
otherNamesFromPrelude
 -- TODO: extend and revise this list
 -- TODO: prelude names are simply existing names?? No need to check??
  = ["GHC.Base.Maybe", "GHC.Base.Just", "GHC.Base.Nothing", "GHC.Base.maybe", "GHC.Base.either", "GHC.Base.not"
    , "Data.Tuple.curry", "Data.Tuple.uncurry", "GHC.Base.compare", "GHC.Base.max", "GHC.Base.min", "GHC.Base.id"]

qualifiedName :: GHC.Name -> String
qualifiedName name = case GHC.nameModule_maybe name of
  Just mod -> GHC.moduleNameString (GHC.moduleName mod) ++ "." ++ GHC.occNameString (GHC.nameOccName name)
  Nothing -> GHC.occNameString (GHC.nameOccName name)

referenceName :: GHC.Name -> LocalRefactor (Ann UName IdDom SrcTemplateStage)
referenceName = referenceName' mkQualName'

referenceOperator :: GHC.Name -> LocalRefactor (Ann UOperator IdDom SrcTemplateStage)
referenceOperator = referenceName' mkQualOp'

-- | Create a name that references the definition. Generates an import if the definition is not yet imported.
referenceName' :: ([String] -> GHC.Name -> Ann nt IdDom SrcTemplateStage) -> GHC.Name 
                    -> LocalRefactor (Ann nt IdDom SrcTemplateStage)
referenceName' makeName name
  | name `elem` registeredNamesFromPrelude || qualifiedName name `elem` otherNamesFromPrelude
  = return $ makeName [] name -- imported from prelude
  | otherwise
  = do RefactorCtx {refCtxRoot = mod, refCtxImports = imports, refModuleName = thisModule} <- ask
       if maybe True (thisModule ==) (GHC.nameModule_maybe name)
         then return $ makeName [] name -- in the same module, use simple name
         else let possibleImports = filter ((name `elem`) . (\imp -> semanticsImported $ imp ^. semantics)) imports
                  fromPrelude = name `elem` semanticsImplicitImports (mod ^. semantics)
               in if | fromPrelude -> return $ makeName [] name
                     | null possibleImports -> do tell [Left name]
                                                  return $ makeName (moduleParts name) name
                     | otherwise -> return $ referenceBy makeName name possibleImports
                                     -- use it according to the best available import
  where moduleParts = maybe [] (splitOn "." . GHC.moduleNameString . GHC.moduleName) . GHC.nameModule_maybe

-- | Reference the name by the shortest suitable import
referenceBy :: ([String] -> GHC.Name -> Ann nt IdDom SrcTemplateStage) -> GHC.Name 
                 -> [Ann UImportDecl IdDom SrcTemplateStage] -> Ann nt IdDom SrcTemplateStage
referenceBy makeName name imps =
  let prefixes = map importQualifier imps
   in makeName (minimumBy (compare `on` (length . concat)) prefixes) name
  where importQualifier :: Ann UImportDecl IdDom SrcTemplateStage -> [String]
        importQualifier imp
          = if isJust (imp ^? importQualified&annJust)
              then case imp ^? importAs&annJust&importRename of
                      Nothing -> splitOn "." (imp ^. importModule&moduleNameString) -- fully qualified import
                      Just asName -> splitOn "." (asName ^. moduleNameString) -- the name given by as clause
              else [] -- unqualified import
