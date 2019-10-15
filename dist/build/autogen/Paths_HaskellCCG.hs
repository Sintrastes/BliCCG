{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskellCCG (
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

bindir     = "/home/nate/.cabal/bin"
libdir     = "/home/nate/.cabal/lib/x86_64-linux-ghc-8.4.4/HaskellCCG-0.1.0.0-K5Tkj80ihMw53eAOdeYz8G"
dynlibdir  = "/home/nate/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/nate/.cabal/share/x86_64-linux-ghc-8.4.4/HaskellCCG-0.1.0.0"
libexecdir = "/home/nate/.cabal/libexec/x86_64-linux-ghc-8.4.4/HaskellCCG-0.1.0.0"
sysconfdir = "/home/nate/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellCCG_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellCCG_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskellCCG_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskellCCG_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellCCG_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellCCG_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
