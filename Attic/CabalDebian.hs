-- |
-- Module      :  Distribution.Package.Debian.Main
-- Copyright   :  David Fox 2008
--
-- Maintainer  :  David Fox <dsf@seereason.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for Debianizer of Cabal packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package.Debian (debian)
import Distribution.Package.Debian.Setup (Flags (..), parseArgs)
import Distribution.Simple.Utils (defaultPackageDesc)
import System.Environment (getArgs)

main :: IO ()

main = do opts <- getArgs >>= parseArgs
          let verbosity = rpmVerbosity opts
          descPath <- defaultPackageDesc verbosity
          pkgDesc <- readPackageDescription verbosity descPath
          debian pkgDesc opts
