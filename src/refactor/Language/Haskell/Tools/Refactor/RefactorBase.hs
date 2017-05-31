{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , ViewPatterns
           , StandaloneDeriving
           , LambdaCase
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , MultiWayIf
           , TemplateHaskell
           , ViewPatterns
           #-}
-- | Basic utilities and types for defining refactorings.
module Language.Haskell.Tools.Refactor.RefactorBase where

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Transform

import Bag as GHC
import DynFlags (HasDynFlags(..))
import ErrUtils as GHC
import Exception (ExceptionMonad(..))
import GHC hiding (mkModuleName, moduleNameString)
import qualified Module as GHC
import qualified Name as GHC
import Outputable
import qualified PrelNames as GHC
import qualified TyCon as GHC
import qualified TysWiredIn as GHC
import SrcLoc

import Control.Exception
import Control.Monad.Reader
import qualified Control.Monad.State as LazySt
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Control.Reference hiding (element)
import Data.Char
import Data.Either
import Data.Function (on)
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Typeable

-- | A type for the input and result of refactoring a module
type UnnamedModule dom = Ann AST.UModule dom SrcTemplateStage

-- | The name of the module and the AST
type ModuleDom dom = (SourceFileKey, UnnamedModule dom)

-- | Module name and marker to separate .hs-boot module definitions. Specifies a source file in a working directory.
data SourceFileKey = SourceFileKey { _sfkFileName :: FilePath
                                   , _sfkModuleName :: String
                                   }
  deriving (Eq, Ord, Show)

-- | A refactoring that only affects one module
type LocalRefactoring dom = UnnamedModule dom -> LocalRefactor dom (UnnamedModule dom)

-- | The type of a refactoring
type Refactoring dom = ModuleDom dom -> [ModuleDom dom] -> Refactor [RefactorChange dom]

-- | Change in the project, modification or removal of a module.
data RefactorChange dom = ContentChanged { fromContentChanged :: ModuleDom dom }
                        | ModuleRemoved { removedModuleName :: String }
                        | ModuleCreated { createdModuleName :: String
                                        , createdModuleContent :: UnnamedModule dom
                                        , sameLocation :: SourceFileKey
                                        }

-- | Exceptions that can occur while loading modules or during internal operations (not during performing the refactor).
data RefactorException = IllegalExtensions [String]
                       | SourceCodeProblem ErrorMessages
                       | ModuleNotInPackage String
                       | UnknownException String
  deriving (Show, Typeable)

moduleSourceFile :: String -> FilePath
moduleSourceFile = undefined

sourceFileModule :: FilePath -> String
sourceFileModule = undefined


-- | The modules of a library, executable, test or benchmark. A package contains one or more module collection.
data ModuleCollection
  = ModuleCollection { _mcId :: ModuleCollectionId
                     , _mcRoot :: FilePath
                     , _mcSourceDirs :: [FilePath]
                     , _mcModules :: Map.Map SourceFileKey ModuleRecord
                     , _mcFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for compiling the modules of this collection
                     , _mcLoadFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for dependency analysis
                     , _mcDependencies :: [ModuleCollectionId]
                     }

-- | The state of a module.
data ModuleRecord
      = ModuleNotLoaded { _recModuleWillNeedCode :: Bool
                        , _recModuleExposed :: Bool
                        }
      | ModuleParsed { _parsedRecModule :: UnnamedModule (Dom RdrName)
                     , _modRecMS :: ModSummary
                     }
      | ModuleRenamed { _renamedRecModule :: UnnamedModule (Dom GHC.Name)
                      , _modRecMS :: ModSummary
                      }
      | ModuleTypeChecked { _typedRecModule :: UnnamedModule IdDom
                          , _modRecMS :: ModSummary
                          }
      | ModuleCodeGenerated { _typedRecModule :: UnnamedModule IdDom
                            , _modRecMS :: ModSummary
                            }

-- | This data structure identifies a module collection.
data ModuleCollectionId = DirectoryMC FilePath
                       | LibraryMC String
                       | ExecutableMC String String
                       | TestSuiteMC String String
                       | BenchmarkMC String String
 deriving (Eq, Ord, Show)

-- | A common class for the state of refactoring tools
class IsRefactSessionState st where
  refSessMCs :: Simple Lens st [ModuleCollection]
  initSession :: st

findModule :: IsRefactSessionState st => String -> StateT st m [FilePath]
findModule = undefined


instance Show ErrorMessages where
  show = show . bagToList

instance Exception RefactorException where
  displayException (SourceCodeProblem prob)
    = "Source code problem: " ++ showSDocUnsafe (vcat (pprErrMsgBagWithLoc prob))
  displayException (IllegalExtensions exts)
    = "The following extensions are not allowed: " ++ (concat $ intersperse ", " exts) ++ "."
  displayException (ModuleNotInPackage modName) = "The module is not in the package: " ++ modName
  displayException (UnknownException ex) = "An unexpected problem appeared: " ++ ex ++ "."

instance Show (RefactorChange dom) where
  show (ContentChanged (n, _)) = "ContentChanged (" ++ show n  ++ ")"
  show (ModuleRemoved n) = "ModuleRemoved " ++ n
  show (ModuleCreated n _ other) = "ModuleCreated " ++ n ++ " (" ++ show other ++ ")"

-- | Performs the given refactoring, transforming it into a Ghc action
runRefactor :: ModuleDom dom -> [ModuleDom dom] -> Refactoring dom -> Ghc (Either String [RefactorChange dom])
runRefactor mod mods trf = runExceptT $ trf mod mods

-- | Wraps a refactoring that only affects one module. Performs the per-module finishing touches.
localRefactoring :: HasModuleInfo dom => LocalRefactoring dom -> Refactoring dom
localRefactoring ref (name, mod) _
  = (\m -> [ContentChanged (name, m)]) <$> localRefactoringRes id mod (ref mod)

-- | Transform the result of the local refactoring
localRefactoringRes :: HasModuleInfo dom
                    => ((UnnamedModule dom -> UnnamedModule dom) -> a -> a)
                          -> UnnamedModule dom
                          -> LocalRefactor dom a
                          -> Refactor a
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
              | Just fstElemSpace <- access fstElem -- TODO: is this needed?
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
addGeneratedImports :: [GHC.Name] -> Ann UModule dom SrcTemplateStage -> Ann UModule dom SrcTemplateStage
addGeneratedImports names m = modImports&annListElems .- (++ addImports names) $ m
  where addImports :: [GHC.Name] -> [Ann UImportDecl dom SrcTemplateStage]
        addImports names = map createImport $ groupBy ((==) `on` GHC.nameModule) $ filter (isJust . GHC.nameModule_maybe) $ nub $ sort names

        -- TODO: group names like constructors into correct IESpecs
        createImport :: [GHC.Name] -> Ann UImportDecl dom SrcTemplateStage
        -- works on groupby result, so list is nonempty
        createImport names = mkImportDecl False False False Nothing (mkModuleName $ GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule $ head names)
                                          Nothing (Just $ mkImportSpecList (map (\n -> mkIESpec (mkUnqualName' n) Nothing) names))

-- some instances missing from GHC

instance (GhcMonad m, Monoid s) => GhcMonad (WriterT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance (ExceptionMonad m, Monoid s) => ExceptionMonad (WriterT s m) where
  gcatch w c = WriterT (runWriterT w `gcatch` (runWriterT . c))
  gmask m = WriterT $ gmask (\f -> runWriterT $ m (WriterT . f . runWriterT))

instance (Monad m, HasDynFlags m) => HasDynFlags (StateT s m) where
  getDynFlags = lift getDynFlags

instance (GhcMonad m) => GhcMonad (StateT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance (ExceptionMonad m) => ExceptionMonad (StateT s m) where
  gcatch r c = StateT (\ctx -> runStateT r ctx `gcatch` (flip runStateT ctx . c))
  gmask m = StateT $ \ctx -> gmask (\f -> runStateT (m (\a -> StateT $ \ctx' -> f (runStateT a ctx'))) ctx)

instance (Monad m, HasDynFlags m) => HasDynFlags (LazySt.StateT s m) where
  getDynFlags = lift getDynFlags

instance (GhcMonad m) => GhcMonad (LazySt.StateT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance (ExceptionMonad m) => ExceptionMonad (LazySt.StateT s m) where
  gcatch r c = LazySt.StateT (\ctx -> LazySt.runStateT r ctx `gcatch` (flip LazySt.runStateT ctx . c))
  gmask m = LazySt.StateT $ \ctx -> gmask (\f -> LazySt.runStateT (m (\a -> LazySt.StateT $ \ctx' -> f (LazySt.runStateT a ctx'))) ctx)


instance GhcMonad m => GhcMonad (ReaderT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance ExceptionMonad m => ExceptionMonad (ReaderT s m) where
  gcatch r c = ReaderT (\ctx -> runReaderT r ctx `gcatch` (flip runReaderT ctx . c))
  gmask m = ReaderT $ \ctx -> gmask (\f -> runReaderT (m (\a -> ReaderT $ \ctx' -> f (runReaderT a ctx'))) ctx)

instance GhcMonad m => GhcMonad (ExceptT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance ExceptionMonad m => ExceptionMonad (ExceptT s m) where
  gcatch e c = ExceptT (runExceptT e `gcatch` (runExceptT . c))
  gmask m = ExceptT $ gmask (\f -> runExceptT $ m (ExceptT . f . runExceptT))


-- | Input and output information for the refactoring
newtype LocalRefactorT dom m a = LocalRefactorT { fromRefactorT :: WriterT [Either GHC.Name (SrcSpan, String, String)] (ReaderT (RefactorCtx dom) m) a }
  deriving (Functor, Applicative, Monad, MonadReader (RefactorCtx dom), MonadWriter [Either GHC.Name (SrcSpan, String, String)], MonadIO, HasDynFlags, ExceptionMonad, GhcMonad)

-- | The information a refactoring can use
data RefactorCtx dom = RefactorCtx { refModuleName :: GHC.Module
                                   , refCtxRoot :: Ann UModule dom SrcTemplateStage
                                   , refCtxImports :: [Ann UImportDecl dom SrcTemplateStage]
                                   }

instance MonadTrans (LocalRefactorT dom) where
  lift = LocalRefactorT . lift . lift

-- | A monad that can be used to refactor
class Monad m => RefactorMonad m where
  refactError :: String -> m a
  liftGhc :: Ghc a -> m a

instance RefactorMonad Refactor where
  refactError = throwE
  liftGhc = lift

instance RefactorMonad (LocalRefactor dom) where
  refactError = lift . refactError
  liftGhc = lift . liftGhc

instance RefactorMonad m => RefactorMonad (StateT s m) where
  refactError = lift . refactError
  liftGhc = lift . liftGhc

instance RefactorMonad m => RefactorMonad (LazySt.StateT s m) where
  refactError = lift . refactError
  liftGhc = lift . liftGhc

-- | The refactoring monad for a given module
type LocalRefactor dom = LocalRefactorT dom Refactor

-- | The refactoring monad for the whole project
type Refactor = ExceptT String Ghc

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

referenceName :: (HasImportInfo dom, HasModuleInfo dom) => GHC.Name -> LocalRefactor dom (Ann UName dom SrcTemplateStage)
referenceName = referenceName' mkQualName'

referenceOperator :: (HasImportInfo dom, HasModuleInfo dom) => GHC.Name -> LocalRefactor dom (Ann UOperator dom SrcTemplateStage)
referenceOperator = referenceName' mkQualOp'

-- | Create a name that references the definition. Generates an import if the definition is not yet imported.
referenceName' :: (HasImportInfo dom, HasModuleInfo dom)
               => ([String] -> GHC.Name -> Ann nt dom SrcTemplateStage) -> GHC.Name -> LocalRefactor dom (Ann nt dom SrcTemplateStage)
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
                                                  return $ makeName [] name
                     | otherwise -> return $ referenceBy makeName name possibleImports
                                     -- use it according to the best available import

-- | Reference the name by the shortest suitable import
referenceBy :: ([String] -> GHC.Name -> Ann nt dom SrcTemplateStage) -> GHC.Name -> [Ann UImportDecl dom SrcTemplateStage] -> Ann nt dom SrcTemplateStage
referenceBy makeName name imps =
  let prefixes = map importQualifier imps
   in makeName (minimumBy (compare `on` (length . concat)) prefixes) name
  where importQualifier :: Ann UImportDecl dom SrcTemplateStage -> [String]
        importQualifier imp
          = if isJust (imp ^? importQualified&annJust)
              then case imp ^? importAs&annJust&importRename of
                      Nothing -> splitOn "." (imp ^. importModule&moduleNameString) -- fully qualified import
                      Just asName -> splitOn "." (asName ^. moduleNameString) -- the name given by as clause
              else [] -- unqualified import

-- | Different classes of definitions that have different kind of names.
data NameClass = Variable         -- ^ Normal value definitions: functions, variables
               | Ctor             -- ^ Data constructors
               | ValueOperator    -- ^ Functions with operator-like names
               | DataCtorOperator -- ^ Constructors with operator-like names
               | SynonymOperator  -- ^ UType definitions with operator-like names

-- | Get which category does a given name belong to
classifyName :: RefactorMonad m => GHC.Name -> m NameClass
classifyName n = liftGhc (lookupName n) >>= return . \case
    Just (AnId {}) | isop     -> ValueOperator
    Just (AnId {})            -> Variable
    Just (AConLike {}) | isop -> DataCtorOperator
    Just (AConLike {})        -> Ctor
    Just (ATyCon {}) | isop   -> SynonymOperator
    Just (ATyCon {})          -> Ctor
    Just (ACoAxiom {})        -> error "classifyName: ACoAxiom"
    Nothing | isop            -> ValueOperator
    Nothing                   -> Variable
  where isop = GHC.isSymOcc (GHC.getOccName n)

-- | Checks if a given name is a valid module name
validModuleName :: String -> Maybe String
validModuleName s = foldl mappend mempty $ map (nameValid Ctor) (splitOn "." s)

-- | Check if a given name is valid for a given kind of definition
nameValid :: NameClass -> String -> Maybe String
nameValid _ "" = Just "An empty name is not valid"
nameValid _ str | str `elem` reservedNames = Just $ "'" ++ str ++ "' is a reserved name"
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
-- Operators that are data constructors (must start with ':')
nameValid DataCtorOperator (':' : nameRest) | all isOperatorChar nameRest = Nothing
nameValid DataCtorOperator _ = Just "A constructor operator must start with ':' and only contain operator characters."
-- Type families and synonyms that are operators (can start with ':')
nameValid SynonymOperator name | all isOperatorChar name = Nothing
nameValid SynonymOperator _ = Just "An operator must only contain operator characters."
-- Normal value operators (cannot start with ':')
nameValid ValueOperator (c : nameRest) | isOperatorChar c && c /= ':' && all isOperatorChar nameRest = Nothing
nameValid ValueOperator _ = Just "An operator that is a value must only contain operator characters and cannot start with ':'"
-- Data and type constructors (start with uppercase)
nameValid Ctor (c : nameRest) | isUpper c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest = Nothing
nameValid Ctor _ = Just "A constructor or module name must start with an uppercase letter, and only contain letters, digits, apostrhophe or underscore"
-- Variables and type variables (start with lowercase)
nameValid Variable (c : nameRest) | isLower c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest = Nothing
nameValid Variable _ = Just "The name of a value must start with lowercase, and only contain letters, digits, apostrhophe or underscore"

isIdStartChar :: Char -> Bool
isIdStartChar c = (isLetter c && isAscii c) || c == '\'' || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = (isPunctuation c || isSymbol c) && isAscii c

makeReferences ''SourceFileKey
