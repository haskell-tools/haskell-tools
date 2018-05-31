{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Tools.Refactor.Querying where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.List ((++), map, find)
import Data.Aeson

import FastString (unpackFS)
import SrcLoc

import GHC (RealSrcSpan, Ghc)
import GHC.Generics (Generic)

import Language.Haskell.Tools.AST (shortShowSpanWithFile)
import Language.Haskell.Tools.Refactor.Prepare (correctRefactorSpan, readSrcSpan)
import Language.Haskell.Tools.Refactor.Representation (ModuleDom)

type QueryType = String
type QueryMonad = ExceptT String Ghc

data QueryValue = GeneralQuery Value
                | MarkerQuery  [Marker]
  deriving (Generic, Show, Eq)

data QueryChoice
  = LocationQuery { queryName     :: String
                  , locationQuery :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
                  }
  | GlobalQuery   { queryName   :: String
                  , globalQuery :: ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
                  }

data Marker = Marker { location :: SrcSpan
                     , severity :: Severity
                     , message :: String
                     } deriving (Generic, Eq)

data Severity = Error | Warning | Info
 deriving (Show, Generic, Eq)

decompQuery :: QueryValue -> (QueryType, Value)
decompQuery (GeneralQuery x) = ("GeneralQuery", x)
decompQuery (MarkerQuery  x) = ("MarkerQuery" , toJSON x)

queryCommands :: [QueryChoice] -> [String]
queryCommands = map queryName

queryError :: String -> QueryMonad a
queryError = throwE

performQuery :: [QueryChoice] -- ^ The set of available queries
                  -> [String] -- ^ The query command
                  -> Either FilePath ModuleDom -- ^ The module in which the refactoring is performed
                  -> [ModuleDom] -- ^ Other modules
                  -> Ghc (Either String (QueryType, Value))
performQuery queries (name:args) modOrPath mods =
  case (query, modOrPath, args) of
    (Just (LocationQuery _ query), Right mod, sp:_)
      -> runExceptT $ decompQuery <$> query (correctRefactorSpan (snd mod) $ readSrcSpan sp) mod mods
    (Just (LocationQuery _ _), _, _)
      -> return $ Left $ "The query '" ++ name ++ "' needs one argument: a source range"
    (Just (GlobalQuery _ query), Right mod, _)
      -> runExceptT $ decompQuery <$> query mod mods
    (Nothing, _, _)
      -> return $ Left $ "Unknown command: " ++ name
  where query = find ((== name) . queryName) queries


instance ToJSON Marker
instance ToJSON Severity
instance ToJSON QueryValue

instance ToJSON SrcSpan where
  toJSON (RealSrcSpan sp) = object [ "file" .= unpackFS (srcSpanFile sp)
                                   , "startRow" .= srcLocLine (realSrcSpanStart sp)
                                   , "startCol" .= srcLocCol (realSrcSpanStart sp)
                                   , "endRow" .= srcLocLine (realSrcSpanEnd sp)
                                   , "endCol" .= srcLocCol (realSrcSpanEnd sp)
                                   ]
  toJSON _ = Null


instance Show Marker where
  show marker = show (severity marker) ++ " at " ++ shortShowSpanWithFile (location marker) ++ ": " ++ message marker
