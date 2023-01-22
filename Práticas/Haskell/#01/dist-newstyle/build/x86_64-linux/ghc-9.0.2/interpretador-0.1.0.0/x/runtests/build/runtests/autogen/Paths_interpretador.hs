{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_interpretador (
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

bindir     = "/home/rodrigo/.cabal/bin"
libdir     = "/home/rodrigo/.cabal/lib/x86_64-linux-ghc-9.0.2/interpretador-0.1.0.0-inplace-runtests"
dynlibdir  = "/home/rodrigo/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/rodrigo/.cabal/share/x86_64-linux-ghc-9.0.2/interpretador-0.1.0.0"
libexecdir = "/home/rodrigo/.cabal/libexec/x86_64-linux-ghc-9.0.2/interpretador-0.1.0.0"
sysconfdir = "/home/rodrigo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpretador_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpretador_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpretador_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpretador_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpretador_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpretador_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
