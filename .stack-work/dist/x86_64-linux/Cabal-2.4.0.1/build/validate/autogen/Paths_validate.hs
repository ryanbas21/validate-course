{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_validate (
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

bindir     = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/bin"
libdir     = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/lib/x86_64-linux-ghc-8.6.3/validate-0.1.0.0-6Bk4Z44nR4QHBjnhTV4ij7-validate"
dynlibdir  = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/share/x86_64-linux-ghc-8.6.3/validate-0.1.0.0"
libexecdir = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/libexec/x86_64-linux-ghc-8.6.3/validate-0.1.0.0"
sysconfdir = "/home/cheus/programming/haskell/validate/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "validate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "validate_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "validate_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "validate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "validate_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "validate_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
