{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ElipticCurveCrypto (
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

bindir     = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\bin"
libdir     = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\lib\\x86_64-windows-ghc-8.8.3\\ElipticCurveCrypto-0.1.0.0-6UC9o6wcr3d1Tbrs2J1f5k"
dynlibdir  = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\share\\x86_64-windows-ghc-8.8.3\\ElipticCurveCrypto-0.1.0.0"
libexecdir = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\libexec\\x86_64-windows-ghc-8.8.3\\ElipticCurveCrypto-0.1.0.0"
sysconfdir = "C:\\Users\\eagle\\Documents\\haskell\\ElipticCurveCrypto\\.stack-work\\install\\05ce9a27\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ElipticCurveCrypto_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ElipticCurveCrypto_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ElipticCurveCrypto_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ElipticCurveCrypto_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ElipticCurveCrypto_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ElipticCurveCrypto_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
