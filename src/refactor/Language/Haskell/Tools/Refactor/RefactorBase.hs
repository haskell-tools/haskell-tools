{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , ViewPatterns
           , StandaloneDeriving
           #-}
module Language.Haskell.Tools.Refactor.RefactorBase where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import GHC (Ghc, GhcMonad(..))
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
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Writer

-- | The information a refactoring can use
data RefactorCtx a = RefactorCtx { refModuleName :: GHC.Module
                                 , refCtxImports :: [Ann ImportDecl a] 
                                 }

-- | Performs the given refactoring, transforming it into a Ghc action
runRefactor :: (a ~ STWithNames n, TemplateAnnot a) => Ann Module a -> (Ann Module a -> Refactor n (Ann Module a)) -> Ghc (Either String (Ann Module a))
runRefactor mod trf = let init = RefactorCtx (fromJust $ mod ^? semantics&defModuleName) (mod ^? element&modImports&annList)
                       in runExceptT $ runReaderT (addGeneratedImports (runWriterT (fromRefactorT $ trf mod))) init

-- | Adds the imports that bring names into scope that are needed by the refactoring
addGeneratedImports :: (TemplateAnnot a, Monad m) => ReaderT (RefactorCtx a) m (Ann Module a, [GHC.Name]) -> ReaderT (RefactorCtx a) m (Ann Module a)
addGeneratedImports = 
  fmap (\(m,names) -> element&modImports&annListElems .- (++ addImports names) $ m)
  where addImports :: TemplateAnnot a => [GHC.Name] -> [Ann ImportDecl a]
        addImports names = map createImport $ groupBy ((==) `on` GHC.nameModule) $ nub $ sort names

        -- TODO: group names like constructors into correct IESpecs
        createImport :: TemplateAnnot a => [GHC.Name] -> Ann ImportDecl a
        createImport names = mkImportDecl False False False Nothing (mkSimpleName $ GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule $ head names)
                                          Nothing (Just $ mkImportSpecList (map (\n -> mkIeSpec (mkSimpleName' n) Nothing) names))

instance (GhcMonad m, Monoid s) => GhcMonad (WriterT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance (ExceptionMonad m, Monoid s) => ExceptionMonad (WriterT s m) where
  gcatch w c = WriterT (runWriterT w `gcatch` (runWriterT . c))
  gmask m = WriterT $ gmask (\f -> runWriterT $ m (WriterT . f . runWriterT))

instance (Monad m, HasDynFlags m, Monoid s) => HasDynFlags (WriterT s m) where
  getDynFlags = lift getDynFlags

instance GhcMonad m => GhcMonad (ReaderT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance ExceptionMonad m => ExceptionMonad (ReaderT s m) where
  gcatch r c = ReaderT (\ctx -> runReaderT r ctx `gcatch` (flip runReaderT ctx . c))
  gmask m = ReaderT $ \ctx -> gmask (\f -> runReaderT (m (\a -> ReaderT $ \ctx' -> f (runReaderT a ctx'))) ctx)

instance (Monad m, HasDynFlags m) => HasDynFlags (ReaderT s m) where
  getDynFlags = lift getDynFlags

instance GhcMonad m => GhcMonad (ExceptT s m) where
  getSession = lift getSession
  setSession env = lift (setSession env)

instance ExceptionMonad m => ExceptionMonad (ExceptT s m) where
  gcatch e c = ExceptT (runExceptT e `gcatch` (runExceptT . c))
  gmask m = ExceptT $ gmask (\f -> runExceptT $ m (ExceptT . f . runExceptT))

instance (Monad m, HasDynFlags m) => HasDynFlags (ExceptT s m) where
  getDynFlags = lift getDynFlags
  

-- | Input and output information for the refactoring
newtype RefactorT ann m ast = RefactorT { fromRefactorT :: WriterT [GHC.Name] (ReaderT (RefactorCtx ann) m) ast }
  deriving (Functor, Applicative, Monad, MonadReader (RefactorCtx ann), MonadWriter [GHC.Name], MonadIO, HasDynFlags, ExceptionMonad, GhcMonad)

instance MonadTrans (RefactorT ann) where
  lift = RefactorT . lift . lift

refactError :: String -> Refactor n a
refactError = lift . throwE

-- | The refactoring monad
type Refactor n = RefactorT (STWithNames n) (ExceptT String Ghc)

type STWithNames n = NodeInfo (SemanticInfo n) SourceTemplate

type RefactoredModule n = Refactor n (Ann Module (STWithNames n))

registeredNamesFromPrelude :: [GHC.Name]
registeredNamesFromPrelude = GHC.basicKnownKeyNames ++ map GHC.tyConName GHC.wiredInTyCons

otherNamesFromPrelude :: [String]
otherNamesFromPrelude 
 -- TODO: extend and revise this list
  = ["GHC.Base.Maybe", "GHC.Base.Just", "GHC.Base.Nothing", "GHC.Base.maybe", "GHC.Base.either", "GHC.Base.not"
    , "Data.Tuple.curry", "Data.Tuple.uncurry", "GHC.Base.compare", "GHC.Base.max", "GHC.Base.min", "GHC.Base.id"]

qualifiedName :: GHC.Name -> String
qualifiedName name = case GHC.nameModule_maybe name of 
  Just mod -> GHC.moduleNameString (GHC.moduleName mod) ++ "." ++ GHC.occNameString (GHC.nameOccName name)
  Nothing -> GHC.occNameString (GHC.nameOccName name)

referenceName :: (Eq n, GHC.NamedThing n) => n -> Refactor n (Ann Name (STWithNames n))
referenceName = referenceName' mkQualName'

referenceOperator :: (Eq n, GHC.NamedThing n) => n -> Refactor n (Ann Operator (STWithNames n))
referenceOperator = referenceName' mkQualOp'

-- | Create a name that references the definition. Generates an import if the definition is not yet imported.
referenceName' :: (Eq n, GHC.NamedThing n) => ([String] -> GHC.Name -> Ann nt (STWithNames n)) -> n -> Refactor n (Ann nt (STWithNames n))
referenceName' makeName n@(GHC.getName -> name) 
  | name `elem` registeredNamesFromPrelude || qualifiedName name `elem` otherNamesFromPrelude
  = return $ makeName [] name -- imported from prelude
  | otherwise 
  = do RefactorCtx {refCtxImports = imports, refModuleName = thisModule} <- ask
       if maybe True (thisModule ==) (GHC.nameModule_maybe name) 
         then return $ makeName [] name -- in the same module, use simple name
         else let possibleImports = filter ((n `elem`) . (\imp -> fromJust $ imp ^? semantics&importedNames)) imports
               in if null possibleImports 
                    then do tell [name]
                            return $ makeName [] name
                    else return $ referenceBy makeName name possibleImports -- use it according to the best available import

-- | Reference the name by the shortest suitable import
referenceBy :: (TemplateAnnot a) => ([String] -> GHC.Name -> Ann nt a) -> GHC.Name -> [Ann ImportDecl a] -> Ann nt a
referenceBy makeName name imps = 
  let prefixes = map importQualifier imps
   in makeName (minimumBy (compare `on` (length . concat)) prefixes) name
  where importQualifier :: Ann ImportDecl a -> [String]
        importQualifier imp 
          = if isJust (imp ^? element&importQualified&annJust) 
              then case imp ^? element&importAs&annJust&element&importRename&element of 
                      Nothing -> nameElements (imp ^. element&importModule&element) -- fully qualified import
                      Just asName -> nameElements asName -- the name given by as clause
              else [] -- unqualified import