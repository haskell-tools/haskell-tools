{-# LANGUAGE LambdaCase
           , ScopedTypeVariables
           #-}
-- | Handlers for common errors in Haskell-tools daemon.
module Language.Haskell.Tools.Daemon.ErrorHandling where

import Control.Exception
import Control.Monad (Monad(..), when)
import Control.Monad.State.Strict (Monad(..), when)
import Control.Reference hiding (modifyMVarMasked_)
import Data.List
import Data.Maybe (Maybe(..), catMaybes)
import Data.Tuple (snd)
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.IO (IO, hPutStrLn, stderr)
import Bag (bagToList)
import ErrUtils (ErrMsg(..))
import GhcMonad (Session(..))
import HscTypes
import SrcLoc (SrcSpan(..), isGoodSrcSpan)

import Language.Haskell.Tools.Daemon.GetModules (UnsupportedPackage(..))
import Language.Haskell.Tools.Refactor

-- Handlers for exceptions specific to our application.
userExceptionHandlers :: (String -> IO a) -> ([(SrcSpan, String)] -> [String] -> IO a) -> [Handler a]
userExceptionHandlers sendError sendCompProblems =
  [ Handler (\(UnsupportedPackage e) -> sendError ("There are unsupported elements in your package: " ++ e ++ " please correct them before loading them into Haskell-tools."))
  , Handler (\(UnsupportedExtension e) -> sendError ("The extension you use is not supported: " ++ e ++ ". Please check your source and cabal files for the use of that language extension."))
  , Handler (\(SpliceInsertionProblem rng _) -> sendError ("A problem occurred while type-checking the Template Haskell splice at: " ++ shortShowSpanWithFile rng ++ ". Some complex splices cannot be type checked for reasons currently unknown. Please simplify the splice. We are working on this problem."))
  , Handler (\case (BreakUpProblem outer rng _) -> sendError ("The program element at " ++ (if isGoodSrcSpan rng then shortShowSpanWithFile rng else shortShowSpanWithFile (RealSrcSpan outer)) ++ " could not be prepared for refactoring. The most likely reason is preprocessor usage. Only conditional compilation is supported, includes and preprocessor macros are not. If there is no preprocessor usage at the given location, there might be a weirdly placed comment causing a problem."))
  , Handler (\(TransformationProblem msg) -> sendError ("A problem occurred while preparing the program for refactoring: " ++ msg))
  , Handler (\(PrettyPrintProblem msg) -> sendError ("A problem occurred while pretty printing the result of the refactoring: " ++ msg))
  , Handler (\case (ConvertionProblem rng msg) -> sendError ("An unexpected problem occurred while converting the representation of the program element at " ++ shortShowSpanWithFile rng ++ ": " ++ msg)
                   (UnrootedConvertionProblem msg) -> sendError ("An unexpected problem occurred while converting between different program representations: " ++ msg))
  , Handler (uncurry sendCompProblems . getProblems)
  ]

-- | Handlers for generic exceptions: 'IOException', 'AsyncException', 'SomeException'.
exceptionHandlers :: IO () -> (String -> IO ()) -> [Handler ()]
exceptionHandlers cont sendError =
  [ Handler (\(err :: IOException) -> hPutStrLn stderr $ "IO Exception caught: " ++ show err)
  , Handler (\(e :: AsyncException) -> hPutStrLn stderr $ "Asynch exception caught: " ++ show e)
  , Handler (\(ex :: SomeException) -> handleException ex cont)
  ]
  where handleException ex cont
          = case handleGHCException (show ex) of
              Nothing -> do hPutStrLn stderr $ "Unexpected error: " ++ show (ex :: SomeException)
                            sendError $ "Internal error: " ++ show ex
              Just (msg, doContinue) -> sendError msg >> when doContinue cont

getProblems :: SourceError -> ([(SrcSpan, String)], [String])
getProblems errs = let msgs = map (\err -> (errMsgSpan err, show err))
                                 $ bagToList $ srcErrorMessages errs
                       hints = nub $ sort $ catMaybes $ map (handleSourceProblem . snd) msgs
                    in (msgs, hints)

-- | Hint text and continuation suggestion for different kinds of errors based on pattern matching on error text.
handleGHCException :: String -> Maybe (String, Bool)
handleGHCException msg | "failed" `isInfixOf` msg && "C pre-processor" `isInfixOf` msg
  = Just ("Failed to load the package. The cause of that is a failure of the pre-processor. "
             ++ " Only conditional compilation is supported, includes and preprocessor macros are not: " ++ msg, False)
handleGHCException msg | "failed" `isInfixOf` msg && "Literate pre-processor" `isInfixOf` msg
  = Just ("Haskell-tools does not handle Literate Haskell yet."
             ++ " If you get this error after refactoring, you should undo the refactoring. The error message: " ++ msg, True)
handleGHCException msg | "cannot satisfy" `isInfixOf` msg
  = Just ("While trying to collect the modules of the project we found the a package is not in the"
             ++ " used package database. Check that you actually compiled this package with all of its components (tests, benchmarks). "
             ++ "If so, make sure that it is installed with the same tools that the project is "
             ++ "configured for (cabal/stack/cabal sandbox). The error message: " ++ msg, True)
handleGHCException msg = Nothing

-- | Hint text and continuation suggestion for different kinds of source problems based on pattern matching on error text.
handleSourceProblem :: String -> Maybe String
handleSourceProblem msg | "is a package module" `isInfixOf` msg
  = Just $ "A module is not found, check that the current working directory is the root of the module hierarchy. "
             ++ " Also check that none of the modules are generated by parser generators or hsc files."
handleSourceProblem msg | "Failed to load interface" `isInfixOf` msg
  = Just $ "Some of the required (external) modules cannot be loaded, because they are not found. Make sure that the project is "
              ++ " already built, and that it is built using the same package database as it is used for refactoring."
handleSourceProblem msg | "Ambiguous interface" `isInfixOf` msg
  = Just $ "Some of the required (external) modules cannot be loaded, because they are ambiguous. "
             ++ "Since there is no separation between packages and package components, make sure that you do not depend "
             ++ "on packages that contain modules with the same qualified name."
handleSourceProblem _
  = Just $ "While loading we found a compilation error in the source code. If it compiles normally "
              ++ "using cabal or stack the problem might result from modules with same name, or "
              ++ "generated files that are not up-to-date."
