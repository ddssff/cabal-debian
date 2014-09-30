{-# OPTIONS -Wall #-}
module Debian.Debianize.Details
    ( debianDefaultAtoms
    , debianVersionSplits
    ) where

import Data.Map as Map (Map, fromList)
import Data.Version (Version(Version))
import Debian.Relation (Relation(Rel), BinPkgName(BinPkgName))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Types.Atoms as T (epochMap, execMap)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude ((++=))
import Debian.Debianize.VersionSplits (DebBase(DebBase), VersionSplits, makePackage, insertSplit)
import Distribution.Package (PackageName(PackageName))

-- | Some details about the debian repository - special cases for how
-- some cabal packages are mapped to debian package names.
debianDefaultAtoms :: Monad m => DebT m ()
debianDefaultAtoms =
    do T.epochMap ++= (PackageName "HaXml", 1)
       T.epochMap ++= (PackageName "HTTP", 1)
       T.execMap ++= ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]])
       mapCabal (PackageName "Cabal") (DebBase "cabal-ghcjs")
       splitCabal (PackageName "Cabal") (DebBase "cabal") (Version [1,21] [])
       mapCabal (PackageName "parsec") (DebBase "parsec3")
       splitCabal (PackageName "parsec") (DebBase "parsec2") (Version [3] [])
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (PackageName "QuickCheck") (DebBase "quickcheck1") (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")

-- | These are the instances of debian names changing that I know
-- about.  I know they really shouldn't be hard coded.  Send a patch.
-- Note that this inherits the lack of type safety of the mkPkgName
-- function.  (FIXME: Use combinators to construct this.)
debianVersionSplits :: Map PackageName VersionSplits
debianVersionSplits =
    Map.fromList
    [ (PackageName "Cabal", makePackage (DebBase "cabal"))
    , (PackageName "parsec", insertSplit (Version [3] []) (DebBase "parsec3") (makePackage (DebBase "parsec2")))
    , (PackageName "QuickCheck", insertSplit (Version [2] []) (DebBase "quickcheck2") (makePackage (DebBase "quickcheck1")))
    -- This looks like a no-op - probably isn't needed.
    , (PackageName "gtk2hs-buildtools", makePackage (DebBase "gtk2hs-buildtools")) ]
