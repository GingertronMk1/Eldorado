{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cabaltest (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jack/.cabal/bin"
libdir     = "/Users/jack/.cabal/lib/x86_64-osx-ghc-8.4.3/cabaltest-0.1.0.0-9pXDxiDvqbkDKDGYM4IbsU-cabaltest"
dynlibdir  = "/Users/jack/.cabal/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/jack/.cabal/share/x86_64-osx-ghc-8.4.3/cabaltest-0.1.0.0"
libexecdir = "/Users/jack/.cabal/libexec/x86_64-osx-ghc-8.4.3/cabaltest-0.1.0.0"
sysconfdir = "/Users/jack/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cabaltest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cabaltest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cabaltest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cabaltest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cabaltest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cabaltest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
