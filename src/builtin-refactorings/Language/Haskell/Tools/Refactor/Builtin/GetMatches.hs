module Language.Haskell.Tools.Refactor.Builtin.GetMatches where

import Language.Haskell.Tools.Refactor

import Control.Monad.Writer
import Control.Reference
import Data.Aeson

import Outputable as GHC
import SrcLoc as GHC
import Id as GHC
import Type as GHC
import TyCon as GHC
import DataCon as GHC

getMatchesQuery :: QueryChoice
getMatchesQuery = LocationQuery "GetMatches" getMatches

getMatches :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getMatches sp (_,mod) _
  = do res <- execWriterT (nodesContaining sp !~ getMatchesOfName $ mod)
       case res of [ctors] -> return ctors
                   _ -> queryError "Not one single results found"


getMatchesOfName :: QualifiedName -> WriterT [Value] QueryMonad QualifiedName
getMatchesOfName n = do ctors <- lift $ getCtors (idType $ semanticsId n)
                        tell [toJSON ctors]
                        return n

getCtors :: GHC.Type -> QueryMonad [(String, [String])]
-- | TODO: unpack forall, context types
-- | TODO: care for infix constructors
getCtors t | Just (tc, _) <- splitTyConApp_maybe t
  = maybe (queryError (noSuccessMsg t)) (return . map formatCtor) (tyConDataCons_maybe tc)
getCtors t = queryError (noSuccessMsg t)

noSuccessMsg :: GHC.Type -> String
noSuccessMsg t = "Cannot find the constructors of type " ++ showSDocUnsafe (ppr t)

formatCtor :: DataCon -> (String, [String])
formatCtor dc = (showSDocUnsafe $ ppr $ dataConName dc, createArgNames (dataConOrigArgTys dc))

-- | TODO: Check for names in scope
-- | TODO: Create names based on the type
createArgNames :: [GHC.Type] -> [String]
createArgNames tys = map (\i -> "p" ++ show i) [1..length tys]
