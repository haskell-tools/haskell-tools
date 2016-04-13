{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , ViewPatterns
           #-}
module Language.Haskell.Tools.Refactor.RefactorBase where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import GHC (Ghc)
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
import Control.Monad.Writer

data RefactorCtx a = RefactorCtx { refModuleName :: GHC.Module
                                 , refCtxImports :: [Ann ImportDecl a] 
                                 }

runRefactor :: (a ~ NodeInfo (SemanticInfo n) s, TemplateAnnot a) => Ann Module a -> (Ann Module a -> Refactor a (Ann Module a)) -> Ghc (Ann Module a)
runRefactor mod trf = let init = RefactorCtx (fromJust $ mod ^? semantics&defModuleName) (mod ^? element&modImports&annList)
                       in runReaderT (addGeneratedImports (runWriterT (fromRefactorT $ trf mod))) init

addGeneratedImports :: TemplateAnnot a => ReaderT (RefactorCtx a) Ghc (Ann Module a, [GHC.Name]) -> ReaderT (RefactorCtx a) Ghc (Ann Module a)
addGeneratedImports = 
  fmap (\(m,names) -> element&modImports&annListElems .- (++ addImports names) $ m)
  where addImports :: TemplateAnnot a => [GHC.Name] -> [Ann ImportDecl a]
        addImports names = map createImport $ groupBy ((==) `on` GHC.nameModule) $ nub $ sort names

        -- TODO: group names like constructors into correct IESpecs
        createImport :: TemplateAnnot a => [GHC.Name] -> Ann ImportDecl a
        createImport names = mkImportDecl False False False Nothing (mkUnqualName $ GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule $ head names)
                                          Nothing (Just $ mkImportSpecList (map (\n -> mkIeSpec (mkUnqualName' n) Nothing) names))

newtype RefactorT m ann ast = RefactorT { fromRefactorT :: WriterT [GHC.Name] (ReaderT (RefactorCtx ann) m) ast }
  deriving (Functor, Applicative, Monad, MonadReader (RefactorCtx ann), MonadWriter [GHC.Name])
type Refactor = RefactorT Ghc

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

referenceName :: (a ~ NodeInfo (SemanticInfo n) s, Eq n, GHC.NamedThing n, TemplateAnnot a) => n -> Refactor a (Ann Name a)
referenceName (GHC.getName -> name) | name `elem` registeredNamesFromPrelude || qualifiedName name `elem` otherNamesFromPrelude
  = return $ mkUnqualName' name -- imported from prelude
referenceName n@(GHC.getName -> name) = do 
  RefactorCtx {refCtxImports = imports} <- ask
  if isNothing (GHC.nameModule_maybe name) 
    then return $ mkUnqualName' name -- in the same module, use simple name
    else let possibleImports = filter ((n `elem`) . (\imp -> fromJust $ imp ^? semantics&importedNames)) imports
          in if null possibleImports 
               then do tell [name] -- have to import itreturn $ mkUnqualName' name
                       return $ mkUnqualName' name
               else return $ referenceBy name possibleImports -- use it according to the best available import


-- | Reference the name by the shortest suitable import
referenceBy :: (TemplateAnnot a) => GHC.Name -> [Ann ImportDecl a] -> Ann Name a
referenceBy name imps = 
  let prefixes = map importQualifier imps
   in mkQualifiedName' (minimumBy (compare `on` (length . concat)) prefixes) name
  where importQualifier :: Ann ImportDecl a -> [String]
        importQualifier imp 
          = if isJust (imp ^? element&importQualified&annJust) 
              then case imp ^? element&importAs&annJust&element&importRename&element of 
                      Nothing -> nameElements (imp ^. element&importModule&element) -- fully qualified import
                      Just asName -> nameElements asName -- the name given by as clause
              else [] -- unqualified import