{-# LANGUAGE LambdaCase
           , CPP 
           , TemplateHaskell
           #-}
-- | Generating instances for traversal on semantic information
module Language.Haskell.Tools.AST.TH.SemanticTraversal where

import Control.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.Tools.AST.Ann

-- | Generate the instances for semantic information
deriveSemanticTraversal :: Name -> Q [Dec]
deriveSemanticTraversal nm = reify nm >>= \case
  TyConI dt -> case dt of
    DataD _ tyConName typArgs _ dataCons _ -> 
      createInstance tyConName typArgs dataCons  
    NewtypeD _ tyConName typArgs _ dataCon _ -> 
      createInstance tyConName typArgs [dataCon]  
    _ -> fail "Unsupported data type"
  _ -> fail "Expected the name of a data type or newtype"
  
createInstance :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
createInstance tyConName typArgs dataCons   
    = do clauses <- mapM createClause dataCons
         return [InstanceD Nothing (map (AppT (ConT className) . VarT) astParams)
                           (AppT (ConT className) (foldl AppT (ConT tyConName) (map VarT astParams)))
                           [FunD funName clauses]]
  where -- | Creates a clause for a constructor, the needed context is also generated
        createClause :: Con -> Q Clause
        createClause (RecC conName conArgs) = createClause' conName (map (\(_,_,t) -> t) conArgs)
        createClause (NormalC conName conArgs) = createClause' conName (map snd conArgs)
        createClause (InfixC conArg1 conName conArg2) = createClause' conName [snd conArg1, snd conArg2]
        
        createClause' :: Name -> [Type] -> Q Clause
        createClause' conName args
          = do bindedNames <- replicateM (length args) (newName "p")
               return $ Clause [ VarP trf, ConP conName (map VarP bindedNames) ] 
                               (NormalB (createExpr conName (zipWith processParam bindedNames args))) []

        (tstage:tdom:_) = map getTyVar $ reverse typArgs
        astParams = reverse $ map getTyVar $ drop 2 $ reverse typArgs

        -- | Creates an expression for the body of a smartTrav clause
        -- using the matches created for parameters
        createExpr :: Name -> [Exp] -> Exp
        createExpr ctrName []
          = AppE applPure $ ConE ctrName
        createExpr ctrName (param1:params)
          = foldl (\coll new -> InfixE (Just coll) applStar (Just new)) 
                  (InfixE (Just $ ConE ctrName) applDollar (Just param1))
                  params
        
        applStar = VarE (mkName "Control.Applicative.<*>")
        applDollar = VarE (mkName "Control.Applicative.<$>")
        applPure = VarE (mkName "Control.Applicative.pure")
        
        className = ''SemanticTraversal
        funName = 'semaTraverse
        trf = mkName "f"
       
        -- | Creates the expression and the predicate for a parameter
        processParam :: Name -> Type -> Exp
        processParam arg (AppT (AppT t (VarT dom)) (VarT stage)) | dom == tdom && stage == tstage
          = AppE (AppE (VarE funName) (VarE trf)) (VarE arg)
        processParam arg _ = AppE applPure (VarE arg)

        getTyVar :: TyVarBndr -> Name
        getTyVar (PlainTV tv) = tv
        getTyVar (KindedTV tv _) = tv

thExamine :: Q [Dec] -> Q [Dec]
thExamine decl = do d <- decl
                    runIO (putStrLn (pprint d))
                    return d