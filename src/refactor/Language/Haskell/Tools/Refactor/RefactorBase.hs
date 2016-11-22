{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , ViewPatterns
           , StandaloneDeriving
           , LambdaCase
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor.RefactorBase where

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Transform
import GHC (Ghc, GhcMonad(..), TyThing(..), lookupName)
import Exception (ExceptionMonad(..))
import DynFlags (HasDynFlags(..))
import qualified Name as GHC
import qualified Module as GHC
import qualified PrelNames as GHC
import qualified TyCon as GHC
import qualified TysWiredIn as GHC
import Control.Reference hiding (element)
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Control.Monad.State

type UnnamedModule dom = Ann AST.UModule dom SrcTemplateStage

-- | The name of the module and the AST
type ModuleDom dom = (String, UnnamedModule dom)

-- | A refactoring that only affects one module
type LocalRefactoring dom = UnnamedModule dom -> LocalRefactor dom (UnnamedModule dom)

-- | The type of a refactoring
type Refactoring dom = ModuleDom dom -> [ModuleDom dom] -> Refactor [RefactorChange dom]

-- | Change in the project, modification or removal of a module.
data RefactorChange dom = ContentChanged { fromContentChanged :: (ModuleDom dom) }
                        | ModuleRemoved { removedModuleName :: String }

-- | Performs the given refactoring, transforming it into a Ghc action
runRefactor :: (HasModuleInfo dom) => ModuleDom dom -> [ModuleDom dom] -> Refactoring dom -> Ghc (Either String [RefactorChange dom])
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
     in flip runReaderT init $ do (mod, newNames) <- runWriterT (fromRefactorT trf)
                                  return $ access (addGeneratedImports newNames) mod

-- | Adds the imports that bring names into scope that are needed by the refactoring
addGeneratedImports :: [GHC.Name] -> Ann UModule dom SrcTemplateStage -> Ann UModule dom SrcTemplateStage
addGeneratedImports names m = modImports&annListElems .- (++ addImports names) $ m
  where addImports :: [GHC.Name] -> [Ann UImportDecl dom SrcTemplateStage]
        addImports names = map createImport $ groupBy ((==) `on` GHC.nameModule) $ filter (isJust . GHC.nameModule_maybe) $ nub $ sort names

        -- TODO: group names like constructors into correct IESpecs
        createImport :: [GHC.Name] -> Ann UImportDecl dom SrcTemplateStage
        createImport names = mkImportDecl False False False Nothing (mkModuleName $ GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule $ head names)
                                          Nothing (Just $ mkImportSpecList (map (\n -> mkIESpec (mkUnqualName' n) Nothing) names))

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
newtype LocalRefactorT dom m a = LocalRefactorT { fromRefactorT :: WriterT [GHC.Name] (ReaderT (RefactorCtx dom) m) a }
  deriving (Functor, Applicative, Monad, MonadReader (RefactorCtx dom), MonadWriter [GHC.Name], MonadIO, HasDynFlags, ExceptionMonad, GhcMonad)

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
                     | null possibleImports -> do tell [name]
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
    Just (AnId id) | isop     -> ValueOperator
    Just (AnId id)            -> Variable
    Just (AConLike id) | isop -> DataCtorOperator
    Just (AConLike id)        -> Ctor
    Just (ATyCon id) | isop   -> SynonymOperator
    Just (ATyCon id)          -> Ctor
    Nothing | isop            -> ValueOperator
    Nothing                   -> Variable
  where isop = GHC.isSymOcc (GHC.getOccName n) 

-- | Checks if a given name is a valid module name
validModuleName :: String -> Bool
validModuleName s = all (nameValid Ctor) (splitOn "." s) 

-- | Check if a given name is valid for a given kind of definition
nameValid :: NameClass -> String -> Bool
nameValid n "" = False
nameValid n str | str `elem` reservedNames = False
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
-- Operators that are data constructors (must start with ':')
nameValid DataCtorOperator (':' : nameRest)
  = all isOperatorChar nameRest
-- UType families and synonyms that are operators (can start with ':')
nameValid SynonymOperator (c : nameRest)
  = isOperatorChar c && all isOperatorChar nameRest
-- Normal value operators (cannot start with ':')
nameValid ValueOperator (c : nameRest)
  = isOperatorChar c && c /= ':' && all isOperatorChar nameRest
-- Data and type constructors (start with uppercase)
nameValid Ctor (c : nameRest)
  = isUpper c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest
-- Variables and type variables (start with lowercase)
nameValid Variable (c : nameRest)
  = isLower c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest
nameValid _ _ = False

isIdStartChar c = (isLetter c && isAscii c) || c == '\'' || c == '_'
isOperatorChar c = (isPunctuation c || isSymbol c) && isAscii c