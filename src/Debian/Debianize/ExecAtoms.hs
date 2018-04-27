-- | Things that seem like they could be clients of this library, but
-- are instead included as part of the library.
{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -ddump-minimal-imports #-}
module Debian.Debianize.ExecAtoms
    ( execAtoms
    ) where

import Control.Lens ( over, (%=) )
import Data.Maybe ( fromMaybe )
import Data.Set as Set ( insert )
import Data.Text as Text ( pack )
import qualified Debian.Debianize.DebInfo as D
    ( InstallFile(destName, destDir, execName, sourceDir),
      Atom(InstallTo, InstallCabalExecTo, Install, InstallCabalExec),
      rulesFragments,
      atomSet )
import Debian.Debianize.Monad ( CabalInfo, execCabalM )
import qualified Debian.Debianize.CabalInfo as A ( debInfo )
import qualified Debian.Debianize.BinaryDebDescription as B ()
import Debian.Pretty ( ppShow )
import Debian.Relation ( BinPkgName )
import System.FilePath ( (</>) )

execAtoms :: BinPkgName -> D.InstallFile -> CabalInfo -> CabalInfo
execAtoms b ifile r =
    over (A.debInfo . D.rulesFragments) (Set.insert (pack ("build" </> ppShow b ++ ":: build-ghc-stamp\n"))) .
    fileAtoms b ifile $
    r

fileAtoms :: BinPkgName -> D.InstallFile -> CabalInfo -> CabalInfo
fileAtoms b installFile' r =
    fileAtoms' b (D.sourceDir installFile') (D.execName installFile') (D.destDir installFile') (D.destName installFile') r

fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> CabalInfo -> CabalInfo
fileAtoms' b sourceDir' execName' destDir' destName' r =
    case (sourceDir', execName' == destName') of
      (Nothing, True) -> execCabalM ((A.debInfo . D.atomSet) %= (Set.insert $ D.InstallCabalExec b execName' d)) r
      (Just s, True) -> execCabalM ((A.debInfo . D.atomSet) %= (Set.insert $ D.Install b (s </> execName') d)) r
      (Nothing, False) -> execCabalM ((A.debInfo . D.atomSet) %= (Set.insert $ D.InstallCabalExecTo b execName' (d </> destName'))) r
      (Just s, False) -> execCabalM ((A.debInfo . D.atomSet) %= (Set.insert $ D.InstallTo b (s </> execName') (d </> destName'))) r
    where
      d = fromMaybe "usr/bin" destDir'
