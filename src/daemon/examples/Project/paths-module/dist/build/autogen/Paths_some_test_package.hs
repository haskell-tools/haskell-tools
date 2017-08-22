{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_some_test_package (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,2,3,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\some-test-package-1.2.3.4-6S3unDRx74P1jz6fzwqDpM"
dynlibdir  = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\some-test-package-1.2.3.4"
libexecdir = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\some-test-package-1.2.3.4-6S3unDRx74P1jz6fzwqDpM\\x86_64-windows-ghc-8.0.2\\some-test-package-1.2.3.4"
sysconfdir = "C:\\Users\\nboldi\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "some_test_package_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "some_test_package_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "some_test_package_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "some_test_package_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "some_test_package_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "some_test_package_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
