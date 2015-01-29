-- | Detailed information about the specific repositories such as
-- debian or seereason - in particular how cabal names are mapped to
-- debian.
{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaultAtoms
    ) where

import Data.Version (Version(Version))
import Debian.Relation (Relation(Rel), BinPkgName(BinPkgName))
import Debian.Debianize.DebianName (mapCabal, {-remapCabal,-} splitCabal)
import Debian.Debianize.Types.Atoms as T (epochMap, execMap)
import Debian.Debianize.Monad (CabalT)
import Debian.Debianize.Prelude ((++=))
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Distribution.Package (PackageName(PackageName))

-- | Update the Atoms value in the CabalT state with some details about
-- the debian repository - special cases for how some cabal packages
-- are mapped to debian package names.
debianDefaultAtoms :: Monad m => CabalT m ()
debianDefaultAtoms =
    do -- These are the two epoch names I know about in the debian repo
       T.epochMap ++= (PackageName "HaXml", 1)
       T.epochMap ++= (PackageName "HTTP", 1)
       -- The hsx2hs build tool is in an an eponymous deb
       T.execMap ++= ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]])
       -- For now, use deb names like libghc-cabal-ghcjs-dev for any
       -- Cabal >= 1.21, which is the ghcjs development branch of Cabal.
       mapCabal (PackageName "Cabal") (DebBase "cabal-ghcjs")
       splitCabal (PackageName "Cabal") (DebBase "cabal") (Version [1,21] [])
       -- The parsec debs are suffixed with either "2" or "3"
       mapCabal (PackageName "parsec") (DebBase "parsec3")
       splitCabal (PackageName "parsec") (DebBase "parsec2") (Version [3] [])
       -- Similar split for quickcheck
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (PackageName "QuickCheck") (DebBase "quickcheck1") (Version [2] [])
       -- Something was required for this package at one time - it
       -- looks like a no-op now
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")
       -- Upgrade transformers to 0.4 - no don't!
       -- remapCabal (PackageName "transformers") (DebBase "transformers4")
       -- remapCabal (PackageName "haskeline") (DebBase "haskeline0713")
