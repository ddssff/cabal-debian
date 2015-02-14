-- To run the test: runhaskell -isrc -DMIN_VERSION_Cabal\(a,b,c\)=1 debian/Debianize.hs
--
-- This doesn't actually modify the debianization, it just sees
-- whether the debianization it would have generated matches the one
-- that is already in debian/.  If not it either means a bug was
-- introduced, or the changes are good and need to be checked in.
--
-- Be sure to run it with the local-debian flag turned off!

import OldLens hiding ((%=), (~=))

import Control.Exception (throw)
import Control.Monad.State (get)
import Data.Default (def)
import Data.List (intercalate)
import Data.Map as Map (insert)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (singleton, insert)
import Data.Text as Text (Text, pack)
import Debian.Changes (ChangeLog(ChangeLog))
import Debian.Debianize
{-
import Debian.Debianize (inputChangeLog, inputDebianization)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Finalize (debianize)
import Debian.Debianize.InputCabalPackageDescription (newFlags)
import Debian.Debianize.Types as T
    (changelog, compat, conflicts, control, depends, debianDescription, homepage, packageDescription,
     installCabalExec, sourceFormat, standardsVersion, utilsPackageNameBase, copyright, xDescription)
import Debian.Debianize.Types.Atoms as T (Atoms, newAtoms, DebInfo, debInfo, makeDebInfo, atomSet, Atom(..))
import Debian.Debianize.Monad (Atoms, CabalT, execCabalT, evalCabalT, execDebianT, liftCabal)
import Debian.Debianize.Output (compareDebianization)
import Debian.Debianize.Prelude ((~=), (~?=), (%=), (+=), (++=))
import Debian.Debianize.Types.CopyrightDescription (CopyrightDescription(..), FilesOrLicenseDescription(..), newCopyrightDescription)
import Debian.Debianize.Types.SourceDebDescription (SourceDebDescription)
import Debian.Policy (SourceFormat(Native3), StandardsVersion(StandardsVersion), License(OtherLicense))
-}
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), VersionReq(SLT, GRE), Relations, parseRelations)
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
       log <- newFlags >>= newCabalInfo >>= evalCabalT (liftCabal inputChangeLog >> access (debInfo . changelog))
       old <- newFlags >>= execDebianT inputDebianization . makeDebInfo
       new <- newFlags >>= newCabalInfo >>= execCabalT (debianize (do debianDefaults
                                                                      (debInfo . changelog) ~?= log
                                                                      customize
                                                                      copyFirstLogEntry old))
       diff <- compareDebianization old (getL debInfo new)
       case diff of
         "" -> return ()
         s -> error $ "Debianization mismatch:\n" ++ s
       -- This would overwrite the existing debianization rather than
       -- just make sure it matches:
       -- writeDebianization "." new
    where
      customize :: Monad m => CabalT m ()
      customize =
          do (debInfo . sourceFormat) ~= Just Native3
             (debInfo . control . standardsVersion) ~= Just (StandardsVersion 3 9 3 Nothing)
             (debInfo . compat) ~= Just 9
             (debInfo . utilsPackageNameBase) ~= Just "cabal-debian"
             -- (copyright . debInfo) %= (Just . copyrightFn . fromMaybe def)
             -- (conflicts . relations . binaryDebDescription (BinPkgName "cabal-debian") . debInfo) %= (++ (rels "haskell-debian-utils (<< 3.59)"))
             (debInfo . binaryDebDescription (BinPkgName "cabal-debian") . relations . depends) %= (++ (rels "apt-file, debian-policy, debhelper, haskell-devscripts (>= 0.8.19)"))
             (debInfo . binaryDebDescription (BinPkgName "libghc-cabal-debian-dev") . relations . depends) %= (++ (rels "debian-policy"))
             (debInfo . executable) %= (Map.insert (BinPkgName "cabal-debian-tests") (InstallFile "cabal-debian-tests" Nothing Nothing "cabal-debian-tests"))
             (debInfo . atomSet) %= (Set.insert $ InstallCabalExec (BinPkgName "cabal-debian") "cabal-debian" "usr/bin")
             (debInfo . utilsPackageNameBase) ~= Just "cabal-debian"
             -- extraDevDeps (BinPkgName "debian-policy")
             (debInfo . control . homepage) ~= Just (pack "https://github.com/ddssff/cabal-debian")

rels :: String -> Relations
rels = either (throw . userError . show) id . parseRelations

-- | This copies the first log entry of deb1 into deb2.  Because the
-- debianization process updates that log entry, we need to undo that
-- update in order to get a clean comparison.
copyFirstLogEntry :: Monad m => DebInfo -> CabalT m ()
copyFirstLogEntry src =
    do dst <- get
       let Just (ChangeLog (hd1 : _)) = getL changelog src
           Just (ChangeLog (_ : tl2)) = getL (debInfo . changelog) dst
       (debInfo . changelog) ~= Just (ChangeLog (hd1 : tl2))
{-
    get >>= \ dst -> 
copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)
-}
