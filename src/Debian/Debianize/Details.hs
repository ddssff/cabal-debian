-- | Detailed information about the specific repositories such as
-- debian or seereason - in particular how cabal names are mapped to
-- debian.
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaults
    ) where

import Control.Lens
import Data.Map as Map (insert)
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Monad (CabalT)
import Debian.Debianize.CabalInfo as A (epochMap, debInfo)
import Debian.Debianize.DebInfo as D (execMap)
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName)
import Distribution.Version (mkVersion)
#else
import Data.Version (Version(Version))
import Distribution.Package (PackageName(PackageName))
#endif

-- | Update the CabalInfo value in the CabalT state with some details about
-- the debian repository - special cases for how some cabal packages
-- are mapped to debian package names.
debianDefaults :: Monad m => CabalT m ()
debianDefaults =
    do -- These are the two epoch names I know about in the debian repo
#if MIN_VERSION_Cabal(2,0,0)
       A.epochMap %= Map.insert (mkPackageName "HaXml") 1
       A.epochMap %= Map.insert (mkPackageName "HTTP") 1
#else
       A.epochMap %= Map.insert (PackageName "HaXml") 1
       A.epochMap %= Map.insert (PackageName "HTTP") 1
#endif
       -- Associate some build tools and their corresponding
       -- (eponymous) debian package names
       mapM_ (\name -> (A.debInfo . D.execMap) %= Map.insert name [[Rel (BinPkgName name) Nothing Nothing]])
            ["ghc", "happy", "alex", "hsx2hs"]
       -- The parsec debs are suffixed with either "2" or "3"
#if !MIN_VERSION_base(4,11,1)
-- The deb of ghc-8.4.3 changes the libghc-parsec3-dev name to libghc-parsec-deb
#if MIN_VERSION_Cabal(2,0,0)
       mapCabal (mkPackageName "parsec") (DebBase "parsec3")
#else
       mapCabal (PackageName "parsec") (DebBase "parsec3")
#endif
#endif
       -- Similar split for quickcheck
#if MIN_VERSION_Cabal(2,0,0)
       mapCabal (mkPackageName "QuickCheck") (DebBase "quickcheck2")
#else
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
#endif
       -- Something was required for this package at one time - it
       -- looks like a no-op now
#if MIN_VERSION_Cabal(2,0,0)
       mapCabal (mkPackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")
#else
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")
#endif
       -- Upgrade transformers to 0.4 - no don't!
       -- remapCabal (PackageName "transformers") (DebBase "transformers4")
       -- remapCabal (PackageName "haskeline") (DebBase "haskeline0713")
#if MIN_VERSION_Cabal(2,0,0)
       mapCabal (mkPackageName "haskell-src-exts") (DebBase "src-exts")
       mapCabal (mkPackageName "haskell-src-meta") (DebBase "src-meta")
       mapCabal (mkPackageName "Cabal") (DebBase "cabal")

       mapCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (mkVersion [2])
#else
       mapCabal (PackageName "haskell-src-exts") (DebBase "src-exts")
       mapCabal (PackageName "haskell-src-meta") (DebBase "src-meta")
       mapCabal (PackageName "Cabal") (DebBase "cabal")

       mapCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (Version [2] [])
#endif
