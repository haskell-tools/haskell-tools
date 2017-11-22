{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
-- | Types and instances for monadic refactorings. The refactoring monad provides automatic
-- importing, keeping important source fragments (such as preprocessor pragmas), and providing
-- contextual information for refactorings.
module Language.Haskell.Tools.Refactor.Monad where

import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as LazySt
import Control.Monad.State.Strict
import Control.Monad.Trans (MonadTrans(..), MonadIO)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Writer
import DynFlags (HasDynFlags(..))
import Exception (ExceptionMonad(..))
import GHC hiding (mkModuleName, moduleNameString)

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.Representation (RefactorChange, ModuleDom, UnnamedModule)

-- | A monad that can be used to refactor
class Monad m => RefactorMonad m where
  refactError :: String -> m a
  liftGhc :: Ghc a -> m a

-- | A refactoring that only affects one module
type LocalRefactoring dom = UnnamedModule dom -> LocalRefactor dom (UnnamedModule dom)

-- | The type of a refactoring
type Refactoring dom = ModuleDom dom -> [ModuleDom dom] -> Refactor [RefactorChange dom]

-- | The type of a refactoring that affects the whole project.
type ProjectRefactoring dom = [ModuleDom dom] -> Refactor [RefactorChange dom]

-- | The refactoring monad for a given module
type LocalRefactor dom = LocalRefactorT dom Refactor

-- | The refactoring monad for the whole project
type Refactor = ExceptT String Ghc

-- | Input and output information for the refactoring
-- TODO: use multiple states instead of Either
newtype LocalRefactorT dom m a
  = LocalRefactorT { fromRefactorT :: WriterT [Either GHC.Name (SrcSpan, String, String)]
                                              (ReaderT (RefactorCtx dom) m) a
                   }
  deriving ( Functor, Applicative, Monad, MonadReader (RefactorCtx dom)
           , MonadWriter [Either GHC.Name (SrcSpan, String, String)]
           , MonadIO, HasDynFlags, ExceptionMonad, GhcMonad )

-- | The information a refactoring can use
data RefactorCtx dom
  = RefactorCtx { refModuleName :: GHC.Module -- ^ The name of the module being refactored. Used for accessing implicit imports.
                , refCtxRoot :: Ann UModule dom SrcTemplateStage
                , refCtxImports :: [Ann UImportDecl dom SrcTemplateStage]
                }

instance MonadTrans (LocalRefactorT dom) where
  lift = LocalRefactorT . lift . lift

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

-- * Some instances missing from GHC

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
