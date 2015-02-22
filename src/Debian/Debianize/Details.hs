-- | Detailed information about the specific repositories such as
-- debian or seereason - in particular how cabal names are mapped to
-- debian.
{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaults
    ) where

import Control.Category ((.))
import Data.Version (Version(Version))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Monad (CabalT)
import Debian.Debianize.Prelude ((++=))
import Debian.Debianize.CabalInfo as A (epochMap, debInfo)
import Debian.Debianize.DebInfo as D (execMap)
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Package (PackageName(PackageName))
import Prelude hiding ((.))

-- | Update the CabalInfo value in the CabalT state with some details about
-- the debian repository - special cases for how some cabal packages
-- are mapped to debian package names.
debianDefaults :: Monad m => CabalT m ()
debianDefaults =
    do -- These are the two epoch names I know about in the debian repo
       A.epochMap ++= (PackageName "HaXml", 1)
       A.epochMap ++= (PackageName "HTTP", 1)
       -- The hsx2hs build tool is in an an eponymous deb
       (A.debInfo . D.execMap) ++= ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]])
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
