{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_associative (
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

bindir     = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\bin"
libdir     = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\lib\\x86_64-windows-ghc-8.6.5\\associative-0.1.0.0-3DsvFuqwg0eISZBwAyQjC6-associative"
dynlibdir  = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\share\\x86_64-windows-ghc-8.6.5\\associative-0.1.0.0"
libexecdir = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\libexec\\x86_64-windows-ghc-8.6.5\\associative-0.1.0.0"
sysconfdir = "C:\\Users\\subnormalseries\\git\\haskell\\chapter15\\Madness\\associative\\.stack-work\\install\\cac820f2\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "associative_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "associative_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "associative_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "associative_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "associative_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "associative_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
