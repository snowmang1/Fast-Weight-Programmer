{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fwp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/bin"
libdir     = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/lib/aarch64-osx-ghc-9.4.7/fwp-0.1.0.0-1qW6pnU8mgWARc1Ex2OTmZ"
dynlibdir  = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/share/aarch64-osx-ghc-9.4.7/fwp-0.1.0.0"
libexecdir = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/libexec/aarch64-osx-ghc-9.4.7/fwp-0.1.0.0"
sysconfdir = "/Users/evandrake/Documents/Thesis/experiments/fwp/code/fwp/.stack-work/install/aarch64-osx/6b561976baff3d25dc7dcbdcb325a38a89b5dae967701ab944eed51330680380/9.4.7/etc"

getBinDir     = catchIO (getEnv "fwp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "fwp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "fwp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "fwp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fwp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fwp_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
