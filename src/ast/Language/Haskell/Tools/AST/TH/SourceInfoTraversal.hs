{-# LANGUAGE LambdaCase
           , CPP 
           , TemplateHaskell
           #-}
-- | Generating instances for traversal on semantic information
module Language.Haskell.Tools.AST.TH.SourceInfoTraversal where

import Control.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.Tools.AST.Ann

-- | Generate the instances for semantic information
deriveSourceInfoTraversal :: Name -> Q [Dec]
deriveSourceInfoTraversal nm = reify nm >>= \case
  TyConI dt -> case dt of
    DataD _ tyConName typArgs _ dataCons _ -> 
      createInstance tyConName typArgs dataCons  
    NewtypeD _ tyConName typArgs _ dataCon _ -> 
      createInstance tyConName typArgs [dataCon]  
    _ -> fail "Unsupported data type"
  _ -> fail "Expected the name of a data type or newtype"
  
createInstance :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
createInstance tyConName typArgs dataCons   
    = do simpleClauses <- mapM (createClause simpleFunName False) dataCons
         upClauses <- mapM (createClause upFunName True) dataCons
         downClauses <- mapM (createClause downFunName True) dataCons
         return [InstanceD Nothing (map (AppT (ConT className) . VarT) astParams)
                           (AppT (ConT className) (foldl AppT (ConT tyConName) (map VarT astParams)))
                           [FunD simpleFunName simpleClauses, FunD upFunName upClauses, FunD downFunName downClauses]]
  where -- | Creates a clause for a constructor, the needed context is also generated
        createClause :: Name -> Bool -> Con -> Q Clause
        createClause funName updown (RecC conName conArgs) = createClause' funName updown conName (map (\(_,_,t) -> t) conArgs)
        createClause funName updown (NormalC conName conArgs) = createClause' funName updown conName (map snd conArgs)
        createClause funName updown (InfixC conArg1 conName conArg2) = createClause' funName updown conName [snd conArg1, snd conArg2]
        
        createClause' :: Name -> Bool -> Name -> [Type] -> Q Clause
        createClause' funName updown conName args
          = do bindedNames <- replicateM (length args) (newName "p")
               return $ Clause ([ VarP trf ] ++ (if updown then [VarP desc, VarP asc] else []) ++ [ ConP conName (map VarP bindedNames) ])
                               (NormalB (createExpr conName (zipWith (processParam funName updown) bindedNames args))) []

        (tstage:tdom:_) = map getTyVar $ reverse typArgs
        astParams = reverse $ map getTyVar $ drop 2 $ reverse typArgs

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
        
        className = ''SourceInfoTraversal
        simpleFunName = 'sourceInfoTraverse
        upFunName = 'sourceInfoTraverseUp
        downFunName = 'sourceInfoTraverseDown
        trf = mkName "trf"
        desc = mkName "desc"
        asc = mkName "asc"
       
        -- | Creates the expression and the predicate for a parameter
        processParam :: Name -> Bool -> Name -> Type -> Exp
        processParam funName updown arg (AppT (AppT t (VarT dom)) (VarT stage)) | dom == tdom && stage == tstage
          = AppE (wrapUpdown $ AppE (VarE funName) (VarE trf)) (VarE arg)
          where wrapUpdown | updown    = flip AppE (VarE asc) . flip AppE (VarE desc)
                           | otherwise = id
        processParam _ _ arg _ = AppE applPure (VarE arg)

        getTyVar :: TyVarBndr -> Name
        getTyVar (PlainTV tv) = tv
        getTyVar (KindedTV tv _) = tv

thExamine :: Q [Dec] -> Q [Dec]
thExamine decl = do d <- decl
                    runIO (putStrLn (pprint d))
                    return d