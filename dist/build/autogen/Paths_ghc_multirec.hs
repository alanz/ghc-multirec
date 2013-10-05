module Paths_ghc_multirec (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alanz/.cabal/bin"
libdir     = "/home/alanz/.cabal/lib/x86_64-linux-ghc-7.6.3/ghc-multirec-0.1.0.0"
datadir    = "/home/alanz/.cabal/share/x86_64-linux-ghc-7.6.3/ghc-multirec-0.1.0.0"
libexecdir = "/home/alanz/.cabal/libexec"
sysconfdir = "/home/alanz/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghc_multirec_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghc_multirec_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ghc_multirec_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghc_multirec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghc_multirec_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
