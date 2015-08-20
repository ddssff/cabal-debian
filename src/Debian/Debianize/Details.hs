-- | Detailed information about the specific repositories such as
-- debian or seereason - in particular how cabal names are mapped to
-- debian.
{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaults
    ) where

import Control.Lens
import Data.Map as Map (insert)
import Data.Version (Version(Version))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Monad (CabalT)
import Debian.Debianize.CabalInfo as A (epochMap, debInfo)
import Debian.Debianize.DebInfo as D (execMap)
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Package (PackageName(PackageName))

-- | Update the CabalInfo value in the CabalT state with some details about
-- the debian repository - special cases for how some cabal packages
-- are mapped to debian package names.
debianDefaults :: Monad m => CabalT m ()
debianDefaults =
    do -- These are the two epoch names I know about in the debian repo
       A.epochMap %= Map.insert (PackageName "HaXml") 1
       A.epochMap %= Map.insert (PackageName "HTTP") 1
       -- Associate some build tools and their corresponding
       -- (eponymous) debian package names
       mapM (\name -> (A.debInfo . D.execMap) %= Map.insert name [[Rel (BinPkgName name) Nothing Nothing]])
            ["ghc", "happy", "alex", "hsx2hs"]
       -- The parsec debs are suffixed with either "2" or "3"
       mapCabal (PackageName "parsec") (DebBase "parsec3")
       -- Similar split for quickcheck
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       -- Something was required for this package at one time - it
       -- looks like a no-op now
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")
       -- Upgrade transformers to 0.4 - no don't!
       -- remapCabal (PackageName "transformers") (DebBase "transformers4")
       -- remapCabal (PackageName "haskeline") (DebBase "haskeline0713")
       mapCabal (PackageName "haskell-src-exts") (DebBase "src-exts")
       mapCabal (PackageName "haskell-src-meta") (DebBase "src-meta")
       mapCabal (PackageName "Cabal") (DebBase "cabal")
