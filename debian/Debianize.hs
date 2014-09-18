-- To run the test: runhaskell -isrc -DMIN_VERSION_Cabal\(a,b,c\)=1 debian/Debianize.hs
--
-- This doesn't actually modify the debianization, it just sees
-- whether the debianization it would have generated matches the one
-- that is already in debian/.  If not it either means a bug was
-- introduced, or the changes are good and need to be checked in.
--
-- Be sure to run it with the local-debian flag turned off!

import Control.Monad.State (get)
import Data.Lens.Lazy (getL, access)
import Data.List (intercalate)
import Data.Monoid (mempty)
import Data.Set (singleton)
import Data.Text as Text (pack)
import Debian.Changes (ChangeLog(ChangeLog))
import Debian.Debianize (inputChangeLog, inputDebianization)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Finalize (debianization)
import Debian.Debianize.Types as T
    (changelog, compat, conflicts, control, depends, debianDescription, homepage,
     installCabalExec, sourceFormat, standardsVersion, utilsPackageNameBase, copyright, xDescription)
import Debian.Debianize.Types.Atoms as T (Atoms, newAtoms, EnvSet(EnvSet))
import Debian.Debianize.Monad (Atoms, DebT, execDebT, evalDebT, execDebM)
import Debian.Debianize.Output (compareDebianization)
import Debian.Debianize.Prelude ((~=), (~?=), (%=), (+=), (++=))
import Debian.Debianize.Types.SourceDebDescription (SourceDebDescription)
import Debian.Policy (SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), VersionReq(SLT, GRE))
import Debian.Version (parseDebianVersion)
import Distribution.Compiler(CompilerFlavor(GHC))
import Prelude hiding (log)
import System.Directory (copyFile)

main :: IO ()
main =
    do -- Copy the changelog into the top directory so that hackage
       -- will see it.
       copyFile "debian/changelog" "changelog"
       -- This is both a debianization script and a unit test - it
       -- makes sure the debianization generated matches the one
       -- checked into version control.
       log <- newAtoms >>= evalDebT (inputChangeLog >> access changelog)
       old <- newAtoms >>= execDebT (inputDebianization (T.EnvSet "/" "/" "/"))
       new <- newAtoms >>= execDebT (debianization debianDefaultAtoms (changelog ~?= log >> customize >> copyFirstLogEntry old))
       diff <- compareDebianization old new
       case diff of
         "" -> return ()
         s -> error $ "Debianization mismatch:\n" ++ s
       -- This would overwrite the existing debianization rather than
       -- just make sure it matches:
       -- writeDebianization "." new
    where
      customize :: Monad m => DebT m ()
      customize =
          do sourceFormat ~= Just Native3
             standardsVersion ~= Just (StandardsVersion 3 9 3 Nothing)
             compat ~= Just 9
             copyright ~= Just (pack (unlines [ "This package is not part of the Debian GNU/Linux distribution."
                                              , ""
                                              , "Copyright: (c) 2010-2011, SeeReason Partners LLC"
                                              , "License: All Rights Reserved"]))
             conflicts (BinPkgName "cabal-debian") %= (++ [[Rel (BinPkgName "haskell-debian-utils") (Just (SLT (parseDebianVersion ("3.59" :: String)))) Nothing]])
             depends (BinPkgName "cabal-debian") %= (++ [[Rel (BinPkgName "apt-file") Nothing Nothing]])
             depends (BinPkgName "cabal-debian") %= (++ [[Rel (BinPkgName "debian-policy") Nothing Nothing]])
             depends (BinPkgName "libghc-cabal-debian-dev") %= (++ [[Rel (BinPkgName "debian-policy") Nothing Nothing]])
             depends (BinPkgName "cabal-debian") %= (++ [[Rel (BinPkgName "debhelper") Nothing Nothing]])
             depends (BinPkgName "cabal-debian") %= (++ [[Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8.19" :: String)))) Nothing]])
             installCabalExec (BinPkgName "cabal-debian-tests") "cabal-debian-tests" "/usr/bin"
             installCabalExec (BinPkgName "cabal-debian") "cabal-debian" "/usr/bin"
             utilsPackageNameBase ~= Just "cabal-debian"
             -- extraDevDeps (BinPkgName "debian-policy")
             homepage ~= Just (pack "http://src.seereason.com/cabal-debian")

-- | This copies the first log entry of deb1 into deb2.  Because the
-- debianization process updates that log entry, we need to undo that
-- update in order to get a clean comparison.
copyFirstLogEntry :: Monad m => Atoms -> DebT m ()
copyFirstLogEntry src =
    do dst <- get
       let Just (ChangeLog (hd1 : _)) = getL T.changelog src
           Just (ChangeLog (_ : tl2)) = getL T.changelog dst
       changelog ~= Just (ChangeLog (hd1 : tl2))
{-
    get >>= \ dst -> 
copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)
-}
