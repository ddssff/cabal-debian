{-# LANGUAGE CPP, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Main
    ( tests
    , main
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Function (on)
import Data.Lens.Lazy (getL, setL)
import Data.List (sortBy)
import Data.Map as Map (differenceWithKey, intersectionWithKey)
import qualified Data.Map as Map (elems, Map, toList)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Set as Set (fromList, union, insert)
import Data.Text as Text (intercalate, split, Text, unlines, unpack)
import Data.Version (Version(Version))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..), parseEntry)
import Debian.Debianize.BasicInfo (compilerFlavor, newFlags)
import qualified Debian.Debianize.BinaryDebDescription as B
import Debian.Debianize.CabalInfo as A
import Debian.Debianize.CopyrightDescription
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.Finalize (debianize {-, finalizeDebianization-})
import Debian.Debianize.Goodies (doBackups, doExecutable, doServer, doWebsite, tightDependencyFixup)
import Debian.Debianize.InputDebian (inputDebianization)
import Debian.Debianize.Monad (CabalT, evalCabalT, execCabalM, execCabalT, liftCabal, execDebianT, DebianT, evalDebianT)
import Debian.Debianize.Prelude ((%=), (++=), (+=), (~=), withCurrentDirectory)
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Pretty (ppDisplay)
import Debian.Policy (databaseDirectory, PackageArchitectures(All), PackagePriority(Extra), parseMaintainer, Section(MainSection), SourceFormat(Native3), StandardsVersion(..), getDebhelperCompatLevel, getDebianStandardsVersion, License(..))
import Debian.Relation (BinPkgName(..), Relation(..), SrcPkgName(..), VersionReq(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Version (parseDebianVersion, buildDebianVersion)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package (PackageName(PackageName))
import Prelude hiding (log, (.))
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit hiding ((~?=))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.HughesPJClass (pPrint, text, Doc)

-- | A suitable defaultAtoms value for the debian repository.
defaultAtoms :: Monad m => CabalT m ()
defaultAtoms =
    do A.epochMap ++= (PackageName "HaXml", 1)
       A.epochMap ++= (PackageName "HTTP", 1)
       mapCabal (PackageName "parsec") (DebBase "parsec3")
       splitCabal (PackageName "parsec") (DebBase "parsec2") (Version [3] [])
       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (PackageName "QuickCheck") (DebBase "quickcheck1") (Version [2] [])
       mapCabal (PackageName "gtk2hs-buildtools") (DebBase "gtk2hs-buildtools")

-- | Force the compiler version to 7.6 to get predictable outputs
testAtoms :: IO CabalInfo
testAtoms = newFlags >>= newCabalInfo >>= return . ghc763
    where
      ghc763 :: CabalInfo -> CabalInfo
      ghc763 atoms = setL (compilerFlavor . D.flags . A.debInfo) GHC atoms

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: Monad m => ChangeLog -> Maybe Int -> Maybe StandardsVersion -> CabalT m ()
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    do (D.changelog . A.debInfo) ~= Just log
       (D.compat . A.debInfo) ~= level
       (S.source . D.control . A.debInfo) ~= Just (SrcPkgName (logPackage entry))
       (S.maintainer . D.control . A.debInfo) ~= either error Just (parseMaintainer (logWho entry))
       (S.standardsVersion . D.control . A.debInfo) ~= standards
newDebianization _ _ _ = error "Invalid changelog"

newDebianization' :: Monad m => Maybe Int -> Maybe StandardsVersion -> CabalT m ()
newDebianization' level standards =
    do (D.compat . A.debInfo) ~= level
       (S.standardsVersion . D.control . A.debInfo) ~= standards

tests :: Test
tests = TestLabel "Debianization Tests" (TestList [-- 1 and 2 do not input a cabal package - we're not ready to
                                                   -- debianize without a cabal package.
                                                   {- test1 "test1",
                                                   test2 "test2", -}
                                                   -- test3 "test3", -- not a cabal package
                                                   test4 "test4 - test-data/clckwrks-dot-com",
                                                   test5 "test5 - test-data/creativeprompts",
                                                   test6 "test6 - test-data/artvaluereport2",
                                                   test7 "test7 - debian/Debianize.hs",
                                                   test8 "test8 - test-data/artvaluereport-data",
                                                   test9 "test9 - test-data/alex",
                                                   test10 "test10 - test-data/archive" {- ,
                                                   issue23 "issue23" -}])

issue23 :: String -> Test
issue23 label =
    TestLabel label $
    TestCase (withCurrentDirectory "test-data/alex/input" $
              do atoms <- testAtoms
                 actual <- evalCabalT (do (D.changelog . A.debInfo) ~= Just (ChangeLog [testEntry])
                                          (D.compat . A.debInfo) ~= Just 9
                                          (D.official . A.debInfo) ~= True
                                          Map.toList <$> liftCabal debianizationFileMap) atoms
                 assertEqual label
                   []
                   actual)

#if 0
test1 :: String -> Test
test1 label =
    TestLabel label $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion :: IO (Maybe StandardsVersion)
                 atoms <- testAtoms
                 deb <- execCabalT
                          (do -- let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              (D.copyright . debInfo) %= (\ f -> (\ pkgDesc -> f pkgDesc >>= \ c -> return $ c { _summaryLicense = Just BSD_3_Clause }))
                              -- inputCabalization top
                              finalizeDebianization)
                          atoms
                 diff <- diffDebianizations (getL debInfo (testDeb1 atoms)) (getL debInfo deb)
                 assertEqual label [] diff)
    where
      testDeb1 :: CabalInfo -> CabalInfo
      testDeb1 atoms =
          execCabalM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                (D.rulesHead . debInfo) %= (const (Just (Text.unlines $
                                                [ "#!/usr/bin/make -f"
                                                , ""
                                                , "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                , "include /usr/share/cdbs/1/class/hlibrary.mk" ])))
                (D.compat . debInfo) ~= Just 9 -- This will change as new version of debhelper are released
                (D.copyright . debInfo) %= (\ f -> (\ pkgDesc -> f pkgDesc >>= \ c -> return $ c { _summaryLicense = Just BSD_3_Clause }))
                (S.source . D.control . debInfo) ~= Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                (S.maintainer . D.control . debInfo) ~= Just (NameAddr (Just "David Fox") "dsf@seereason.com")
                (S.standardsVersion . D.control . debInfo) ~= Just (StandardsVersion 3 9 3 (Just 1)) -- This will change as new versions of debian-policy are released
                (S.buildDepends . D.control . debInfo) %=
                                  (++ [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                       [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                       [Rel (BinPkgName "cdbs") Nothing Nothing],
                                       [Rel (BinPkgName "ghc") Nothing Nothing],
                                       [Rel (BinPkgName "ghc-prof") Nothing Nothing]])
                (S.buildDependsIndep . D.control . debInfo) %= (++ [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]))
            atoms
      log = ChangeLog [Entry { logPackage = "haskell-cabal-debian"
                             , logVersion = buildDebianVersion Nothing "2.6.2" Nothing
                             , logDists = [ReleaseName {relName = "unstable"}]
                             , logUrgency = "low"
                             , logComments = "  * Fix a bug constructing the destination pathnames that was dropping\n    files that were supposed to be installed into packages.\n"
                             , logWho = "David Fox <dsf@seereason.com>"
                             , logDate = "Thu, 20 Dec 2012 06:49:25 -0800" }]

test2 :: String -> Test
test2 label =
    TestLabel label $
    TestCase (do level <- getDebhelperCompatLevel
                 standards <- getDebianStandardsVersion
                 atoms <- testAtoms
                 deb <- execCabalT
                          (do -- let top = Top "."
                              defaultAtoms
                              newDebianization (ChangeLog [testEntry]) level standards
                              (D.copyright . debInfo) %= (\ f -> (\ pkgDesc -> f pkgDesc >>= \ c -> return $ c { _summaryLicense = Just BSD_3_Clause }))
                              -- inputCabalization top
                              finalizeDebianization)
                          atoms
                 diff <- diffDebianizations (getL debInfo (expect atoms)) (getL debInfo deb)
                 assertEqual label [] diff)
    where
      expect atoms =
          execCabalM
            (do defaultAtoms
                newDebianization log (Just 9) (Just (StandardsVersion 3 9 3 (Just 1)))
                (D.rulesHead . debInfo) %= (const (Just (Text.unlines $
                                                ["#!/usr/bin/make -f",
                                                 "",
                                                 "include /usr/share/cdbs/1/rules/debhelper.mk",
                                                 "include /usr/share/cdbs/1/class/hlibrary.mk"])))
                (D.compat . debInfo) ~= Just 9
                (D.copyright . debInfo) %= (\ f -> (\ pkgDesc -> f pkgDesc >>= \ c -> return $ c { _summaryLicense = Just BSD_3_Clause }))
                (S.source . D.control . debInfo) ~= Just (SrcPkgName {unSrcPkgName = "haskell-cabal-debian"})
                (S.maintainer . D.control . debInfo) ~= Just (NameAddr {nameAddr_name = Just "David Fox", nameAddr_addr = "dsf@seereason.com"})
                (S.standardsVersion . D.control . debInfo) ~= Just (StandardsVersion 3 9 3 (Just 1))
                (S.buildDepends . D.control . debInfo)
                               %= (++ [[Rel (BinPkgName "debhelper") (Just (GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                                       [Rel (BinPkgName "haskell-devscripts") (Just (GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                                       [Rel (BinPkgName "cdbs") Nothing Nothing],
                                       [Rel (BinPkgName "ghc") Nothing Nothing],
                                       [Rel (BinPkgName "ghc-prof") Nothing Nothing]])
                (S.buildDependsIndep . D.control . debInfo) %= (++ [[Rel (BinPkgName "ghc-doc") Nothing Nothing]]))
            atoms
      log = ChangeLog [Entry {logPackage = "haskell-cabal-debian",
                              logVersion = Debian.Version.parseDebianVersion ("2.6.2" :: String),
                              logDists = [ReleaseName {relName = "unstable"}],
                              logUrgency = "low",
                              logComments = Prelude.unlines ["  * Fix a bug constructing the destination pathnames that was dropping",
                                                             "    files that were supposed to be installed into packages."],
                              logWho = "David Fox <dsf@seereason.com>",
                              logDate = "Thu, 20 Dec 2012 06:49:25 -0800"}]
#endif

testEntry :: ChangeLogEntry
testEntry =
    either (error "Error in test changelog entry") fst
           (parseEntry (Prelude.unlines
                                [ "haskell-cabal-debian (2.6.2) unstable; urgency=low"
                                , ""
                                , "  * Fix a bug constructing the destination pathnames that was dropping"
                                , "    files that were supposed to be installed into packages."
                                , ""
                                , " -- David Fox <dsf@seereason.com>  Thu, 20 Dec 2012 06:49:25 -0800" ]))

test3 :: String -> Test
test3 label =
    TestLabel label $
    TestCase (let top = "test-data/haskell-devscripts" in
              withCurrentDirectory top $
              do atoms <- testAtoms
                 deb <- (execCabalT (liftCabal inputDebianization) atoms)
                 diff <- diffDebianizations (getL debInfo (testDeb2 atoms)) (getL debInfo deb)
                 assertEqual label [] diff)
    where
      testDeb2 :: CabalInfo -> CabalInfo
      testDeb2 atoms =
          execCabalM
            (do defaultAtoms
                newDebianization log (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
                (D.sourceFormat . debInfo) ~= Just Native3
                (D.rulesHead . debInfo) ~=
                               Just (Text.unlines  ["#!/usr/bin/make -f",
                                                    "# -*- makefile -*-",
                                                    "",
                                                    "# Uncomment this to turn on verbose mode.",
                                                    "#export DH_VERBOSE=1",
                                                    "",
                                                    "DEB_VERSION := $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')",
                                                    "",
                                                    "manpages = $(shell cat debian/manpages)",
                                                    "",
                                                    "%.1: %.pod",
                                                    "\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@",
                                                    "",
                                                    "%.1: %",
                                                    "\tpod2man -c 'Haskell devscripts documentation' -r 'Haskell devscripts $(DEB_VERSION)' $< > $@",
                                                    "",
                                                    ".PHONY: build",
                                                    "build: $(manpages)",
                                                    "",
                                                    "install-stamp:",
                                                    "\tdh install",
                                                    "",
                                                    ".PHONY: install",
                                                    "install: install-stamp",
                                                    "",
                                                    "binary-indep-stamp: install-stamp",
                                                    "\tdh binary-indep",
                                                    "\ttouch $@",
                                                    "",
                                                    ".PHONY: binary-indep",
                                                    "binary-indep: binary-indep-stamp",
                                                    "",
                                                    ".PHONY: binary-arch",
                                                    "binary-arch: install-stamp",
                                                    "",
                                                    ".PHONY: binary",
                                                    "binary: binary-indep-stamp",
                                                    "",
                                                    ".PHONY: clean",
                                                    "clean:",
                                                    "\tdh clean",
                                                    "\trm -f $(manpages)",
                                                    "",
                                                    ""])
                (D.compat . debInfo) ~= Just 7
                (D.copyright . debInfo) %= (\ f -> (\ pkgDesc -> f pkgDesc >>= \ c -> return $ c { _summaryComment = Just "This package was debianized by John Goerzen <jgoerzen@complete.org> on\nWed,  6 Oct 2004 09:46:14 -0500.\n\nCopyright information removed from this test data.\n" }))
                (S.source . D.control . debInfo) ~= Just (SrcPkgName {unSrcPkgName = "haskell-devscripts"})
                (S.maintainer . D.control . debInfo) ~= Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                (S.uploaders . D.control . debInfo) ~= [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                (S.priority . D.control . debInfo) ~= Just Extra
                (S.section . D.control . debInfo) ~= Just (MainSection "haskell")
                (S.buildDepends . D.control . debInfo) %= (++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]])
                (S.buildDependsIndep . D.control . debInfo) %=  (++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]])
                (S.standardsVersion . D.control . debInfo) ~= Just (StandardsVersion 3 9 4 Nothing)
                (S.vcsFields . D.control . debInfo) %= Set.union (Set.fromList [ S.VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                       , S.VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                (B.architecture . D.binaryDebDescription (BinPkgName "haskell-devscripts") . debInfo)  ~= Just All
                (B.description . D.binaryDebDescription (BinPkgName "haskell-devscripts") . debInfo) ~=
                   Just
                     (intercalate "\n"   ["Tools to help Debian developers build Haskell packages",
                                          " This package provides a collection of scripts to help build Haskell",
                                          " packages for Debian.  Unlike haskell-utils, this package is not",
                                          " expected to be installed on the machines of end users.",
                                          " .",
                                          " This package is designed to support Cabalized Haskell libraries.  It",
                                          " is designed to build a library for each supported Debian compiler or",
                                          " interpreter, generate appropriate postinst/prerm files for each one,",
                                          " generate appropriate substvars entries for each one, and install the",
                                          " package in the Debian temporary area as part of the build process."])
                (B.depends . B.relations . D.binaryDebDescription (BinPkgName "haskell-devscripts") . debInfo) ~=
                     [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "debhelper"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "dh-buildinfo"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "ghc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.6" :: String)))) Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "html-xml-utils"}) Nothing Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "hscolour"}) (Just (GRE (Debian.Version.parseDebianVersion ("1.8" :: String)))) Nothing]
                     , [Rel (BinPkgName {unBinPkgName = "ghc-haddock"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.4" :: String)))) Nothing] ]
{-
                control %= (\ y -> y { S.source = 
                                     , S.maintainer = Just (NameAddr {nameAddr_name = Just "Debian Haskell Group", nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"})
                                     , S.uploaders = [NameAddr {nameAddr_name = Just "Marco Silva", nameAddr_addr = "marcot@debian.org"},NameAddr {nameAddr_name = Just "Joachim Breitner", nameAddr_addr = "nomeata@debian.org"}]
                                     , S.priority = Just Extra
                                     , S.section = Just (MainSection "haskell")
                                     , S.buildDepends = (S.buildDepends y) ++ [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion ("7" :: String)))) Nothing]]
                                     , S.buildDependsIndep = (S.buildDependsIndep y) ++ [[Rel (BinPkgName {unBinPkgName = "perl"}) Nothing Nothing]]
                                     , S.standardsVersion = Just (StandardsVersion 3 9 4 Nothing)
                                     , S.vcsFields = Set.union (S.vcsFields y) (Set.fromList [ S.VCSBrowser "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-devscripts"
                                                                                                 , S.VCSDarcs "http://darcs.debian.org/pkg-haskell/haskell-devscripts"])
                                     , S.binaryPackages = [S.BinaryDebDescription { B.package = BinPkgName {unBinPkgName = "haskell-devscripts"}
                                                                                      , B.architecture = All
                                                                                      , B.binarySection = Nothing
                                                                                      , B.binaryPriority = Nothing
                                                                                      , B.essential = False
                                                                                      , B.description = Just $
                                                                                          (T.intercalate "\n"
                                                                                           ["Tools to help Debian developers build Haskell packages",
                                                                                            " This package provides a collection of scripts to help build Haskell",
                                                                                            " packages for Debian.  Unlike haskell-utils, this package is not",
                                                                                            " expected to be installed on the machines of end users.",
                                                                                            " .",
                                                                                            " This package is designed to support Cabalized Haskell libraries.  It",
                                                                                            " is designed to build a library for each supported Debian compiler or",
                                                                                            " interpreter, generate appropriate postinst/prerm files for each one,",
                                                                                            " generate appropriate substvars entries for each one, and install the",
                                                                                            " package in the Debian temporary area as part of the build process."])
                                                                                      , B.relations =
                                                                                          B.PackageRelations
                                                                                            { B.depends =
                                                                                              [ [Rel (BinPkgName {unBinPkgName = "dctrl-tools"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "debhelper"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "dh-buildinfo"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "ghc"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.6" :: String)))) Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "${misc:Depends}"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "html-xml-utils"}) Nothing Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "hscolour"}) (Just (GRE (Debian.Version.parseDebianVersion ("1.8" :: String)))) Nothing]
                                                                                              , [Rel (BinPkgName {unBinPkgName = "ghc-haddock"}) (Just (GRE (Debian.Version.parseDebianVersion ("7.4" :: String)))) Nothing] ]
                                                                                            , B.recommends = []
                                                                                            , B.suggests = []
                                                                                            , B.preDepends = []
                                                                                            , B.breaks = []
                                                                                            , B.conflicts = []
                                                                                            , B.provides_ = []
                                                                                            , B.replaces_ = []
                                                                                            , B.builtUsing = [] }}]})
-}
                                                                                            )
            atoms
      log = ChangeLog [Entry { logPackage = "haskell-devscripts"
                             , logVersion = Debian.Version.parseDebianVersion ("0.8.13" :: String)
                             , logDists = [ReleaseName {relName = "experimental"}]
                             , logUrgency = "low"
                             , logComments = "  [ Joachim Breitner ]\n  * Improve parsing of \"Setup register\" output, patch by David Fox\n  * Enable creation of hoogle files, thanks to Kiwamu Okabe for the\n    suggestion. \n\n  [ Kiwamu Okabe ]\n  * Need --html option to fix bug that --hoogle option don't output html file.\n  * Support to create /usr/lib/ghc-doc/hoogle/*.txt for hoogle package.\n\n  [ Joachim Breitner ]\n  * Symlink hoogle\8217s txt files to /usr/lib/ghc-doc/hoogle/\n  * Bump ghc dependency to 7.6 \n  * Bump standards version\n"
                             , logWho = "Joachim Breitner <nomeata@debian.org>"
                             , logDate = "Mon, 08 Oct 2012 21:14:50 +0200" },
                       Entry { logPackage = "haskell-devscripts"
                             , logVersion = Debian.Version.parseDebianVersion ("0.8.12" :: String)
                             , logDists = [ReleaseName {relName = "unstable"}]
                             , logUrgency = "low"
                             , logComments = "  * Depend on ghc >= 7.4, adjusting to its haddock --interface-version\n    behaviour.\n"
                             , logWho = "Joachim Breitner <nomeata@debian.org>"
                             , logDate = "Sat, 04 Feb 2012 10:50:33 +0100"}]

test4 :: String -> Test
test4 label =
    TestLabel label $
    TestCase (do let outTop = "test-data/clckwrks-dot-com/output"
                 let inTop = "test-data/clckwrks-dot-com/input"
                 atoms <- withCurrentDirectory inTop $ testAtoms
                 old <- withCurrentDirectory outTop $ do
                          execCabalT (liftCabal inputDebianization) atoms
                 let log = getL (D.changelog . debInfo) old
                 new <- withCurrentDirectory inTop $ do
                          execCabalT (debianize (defaultAtoms >> customize log)) atoms
                 diff <- diffDebianizations (getL debInfo old) (getL debInfo ({-copyFirstLogEntry old-} new))
                 assertEqual label [] diff)
    where
      customize :: Maybe ChangeLog -> CabalT IO ()
      customize log =
          do (D.changelog . debInfo) ~= log
             liftCabal tight
             fixRules
             doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups"
             doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))
             (D.revision . A.debInfo) ~= Nothing
             (D.missingDependencies . A.debInfo) += (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")
             (D.sourceFormat . debInfo) ~= Just Native3
             (S.homepage . D.control . debInfo) ~= Just "http://www.clckwrks.com/"
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))
{-
      customize log = modifyM (lift . customize' log)
      customize' :: Maybe ChangeLog -> CabalInfo -> IO CabalInfo
      customize' log atoms =
          execCabalT (newDebianization' (Just 7) (Just (StandardsVersion 3 9 4 Nothing))) .
          modL T.control (\ y -> y {T.homepage = Just "http://www.clckwrks.com/"}) .
          setL T.sourceFormat (Just Native3) .
          modL T.missingDependencies (insert (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")) .
          setL T.revision Nothing .
          execCabalM (doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))) .
          execCabalM (doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups") .
          fixRules .
          execCabalM tight .
          setL T.changelog log
-}
      -- A log entry gets added when the Debianization is generated,
      -- it won't match so drop it for the comparison.
      serverNames = map BinPkgName ["clckwrks-dot-com-production"] -- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development"]
      -- Insert a line just above the debhelper.mk include
      fixRules =
          (D.rulesSettings . debInfo) %= (++ ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups"])
{-
          mapAtoms f deb
          where
            f :: DebAtomKey -> DebAtom -> Set (DebAtomKey, DebAtom)
            f Source (DebRulesHead t) =
                singleton (Source, DebRulesHead (T.unlines $ concat $
                                                 map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                                                else [line] :: [T.Text]) (T.lines t)))
            f k a = singleton (k, a)
-}
      tight = mapM_ (tightDependencyFixup [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                                           (BinPkgName "libghc-clckwrks-plugin-bugs-dev", BinPkgName "haskell-clckwrks-plugin-bugs-utils"),
                                           (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) serverNames

      theSite :: BinPkgName -> D.Site
      theSite deb =
          D.Site { D.domain = hostname'
                 , D.serverAdmin = "logic@seereason.com"
                 , D.server = theServer deb }
      theServer :: BinPkgName -> D.Server
      theServer deb =
          D.Server { D.hostname =
                       case deb of
                         BinPkgName "clckwrks-dot-com-production" -> hostname'
                         _ -> hostname'
                 , D.port = portNum deb
                 , D.headerMessage = "Generated by clckwrks-dot-com/Setup.hs"
                 , D.retry = "60"
                 , D.serverFlags =
                     [ "--http-port", show (portNum deb)
                     , "--hide-port"
                     , "--hostname", hostname'
                     , "--top", databaseDirectory deb
                     , "--enable-analytics"
                     , "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path
                     ]
                 , D.installFile =
                     D.InstallFile { D.execName   = "clckwrks-dot-com-server"
                                   , D.destName   = ppDisplay deb
                                   , D.sourceDir  = Nothing
                                   , D.destDir    = Nothing }
                 }
      hostname' = "clckwrks.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "clckwrks-dot-com-production"  -> 9029
            "clckwrks-dot-com-staging"     -> 9038
            "clckwrks-dot-com-development" -> 9039
            _ -> error $ "Unexpected package name: " ++ deb
      jstreePath = "/usr/share/clckwrks-0.13.2/jstree"
      json2Path = "/usr/share/clckwrks-0.13.2/json2"

anyrel :: BinPkgName -> Relation
anyrel b = Rel b Nothing Nothing

test5 :: String -> Test
test5 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/creativeprompts/input"
                     outTop = "test-data/creativeprompts/output"
                 atoms <- withCurrentDirectory inTop testAtoms
                 old <- withCurrentDirectory outTop $ newFlags >>= execDebianT inputDebianization . D.makeDebInfo
                 let standards = getL (S.standardsVersion . D.control) old
                     level = getL D.compat old
                 new <- withCurrentDirectory inTop (execCabalT (debianize (defaultAtoms >> customize old level standards)) atoms)
                 diff <- diffDebianizations old (getL debInfo new)
                 assertEqual label [] diff)
    where
      customize old level standards =
          do (D.utilsPackageNameBase . A.debInfo) ~= Just "creativeprompts-data"
             newDebianization' level standards
             (D.changelog . debInfo) ~= (getL D.changelog old)
             doWebsite (BinPkgName "creativeprompts-production") (theSite (BinPkgName "creativeprompts-production"))
             doServer (BinPkgName "creativeprompts-development") (theServer (BinPkgName "creativeprompts-development"))
             doBackups (BinPkgName "creativeprompts-backups") "creativeprompts-backups"
             (D.execMap . A.debInfo) ++= ("trhsx", [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]])
             mapM_ (\ b -> (B.depends . B.relations . D.binaryDebDescription b . debInfo) %= \ deps -> deps ++ [[anyrel (BinPkgName "markdown")]])
                   [(BinPkgName "creativeprompts-production"), (BinPkgName "creativeprompts-development")]
             (B.description . D.binaryDebDescription (BinPkgName "creativeprompts-development") . debInfo) ~=
                   Just (intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                            , "  Testing version of the blog server, runs on port"
                                            , "  8000 with HTML validation turned on." ])
             (B.description . D.binaryDebDescription (BinPkgName "creativeprompts-data") . debInfo) ~=
                   Just (intercalate "\n" [ "creativeprompts.com data files"
                                            , "  Static data files for creativeprompts.com"])
             (B.description . D.binaryDebDescription (BinPkgName "creativeprompts-production") . debInfo) ~=
                   Just (intercalate "\n" [ "Configuration for running the creativeprompts.com server"
                                            , "  Production version of the blog server, runs on port"
                                            , "  9021 with HTML validation turned off." ])
             (B.description . D.binaryDebDescription (BinPkgName "creativeprompts-backups") . debInfo) ~=
                   Just (intercalate "\n" [ "backup program for creativeprompts.com"
                                            , "  Install this somewhere other than creativeprompts.com to run automated"
                                            , "  backups of the database."])
             (B.architecture . D.binaryDebDescription (BinPkgName "creativeprompts-production") . debInfo) ~= Just All
             (B.architecture . D.binaryDebDescription (BinPkgName "creativeprompts-data") . debInfo) ~= Just All
             (B.architecture . D.binaryDebDescription (BinPkgName "creativeprompts-development") . debInfo) ~= Just All
             (D.sourceFormat . debInfo) ~= Just Native3

      theSite :: BinPkgName -> D.Site
      theSite deb =
          D.Site { D.domain = hostname'
                 , D.serverAdmin = "logic@seereason.com"
                 , D.server = theServer deb }
      theServer :: BinPkgName -> D.Server
      theServer deb =
          D.Server
                 { D.hostname =
                       case deb of
                         BinPkgName "clckwrks-dot-com-production" -> hostname'
                         _ -> hostname'
                 , D.port = portNum deb
                 , D.headerMessage = "Generated by creativeprompts-dot-com/debian/Debianize.hs"
                 , D.retry = "60"
                 , D.serverFlags =
                     [ "--http-port", show (portNum deb)
                     , "--hide-port"
                     , "--hostname", hostname'
                     , "--top", databaseDirectory deb
                     , "--enable-analytics"
                     , "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path
                     ]
                 , D.installFile =
                     D.InstallFile { D.execName   = "creativeprompts-server"
                                 , D.destName   = ppDisplay deb
                                 , D.sourceDir  = Nothing
                                 , D.destDir    = Nothing }
                 }
      hostname' = "creativeprompts.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "creativeprompts-production"  -> 9022
            "creativeprompts-staging"     -> 9033
            "creativeprompts-development" -> 9034
            _ -> error $ "Unexpected package name: " ++ deb
      jstreePath = "/usr/share/clckwrks-0.13.2/jstree"
      json2Path = "/usr/share/clckwrks-0.13.2/json2"

test6 :: String -> Test
test6 label =
    TestLabel label $
    TestCase (do result <- readProcessWithExitCode "runhaskell" ["-isrc", "-DMIN_VERSION_Cabal(a,b,c)=1", "test-data/artvaluereport2/input/debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "") result)

test7 :: String -> Test
test7 label =
    TestLabel label $
    TestCase (do new <- readProcessWithExitCode "runhaskell" ["-isrc", "-DMIN_VERSION_Cabal(a,b,c)=1", "debian/Debianize.hs"] ""
                 assertEqual label (ExitSuccess, "", "Ignored: debian/cabal-debian.1\nIgnored: debian/cabal-debian.manpages\n") new)

test8 :: String -> Test
test8 label =
    TestLabel label $
    TestCase ( do let inTop = "test-data/artvaluereport-data/input"
                      outTop = "test-data/artvaluereport-data/output"
                  (old :: D.DebInfo) <- withCurrentDirectory outTop $ newFlags >>= execDebianT inputDebianization . D.makeDebInfo
                  let log = getL D.changelog old
                  new <- withCurrentDirectory inTop $ newFlags >>= newCabalInfo >>= execCabalT (debianize (defaultAtoms >> customize log))
                  diff <- diffDebianizations old (getL debInfo new)
                  assertEqual label [] diff
             )
    where
      customize Nothing = error "Missing changelog"
      customize (Just log) =
          do (S.buildDepends . D.control . debInfo) %= (++ [[Rel (BinPkgName "haskell-hsx-utils") Nothing Nothing]])
             (S.homepage . D.control . debInfo) ~= Just "http://artvaluereportonline.com"
             (D.sourceFormat . debInfo) ~= Just Native3
             (D.changelog . debInfo) ~= Just log
             newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))

test9 :: String -> Test
test9 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/alex/input"
                     outTop = "test-data/alex/output"
                 new <- withCurrentDirectory inTop $ newFlags >>= newCabalInfo >>= execCabalT (debianize (defaultAtoms >> customize))
                 let Just (ChangeLog (entry : _)) = getL (D.changelog . debInfo) new
                 old <- withCurrentDirectory outTop $ newFlags >>= execDebianT (inputDebianization >> copyChangelogDate (logDate entry)) . D.makeDebInfo
                 diff <- diffDebianizations old (getL debInfo new)
                 assertEqual label [] diff)
    where
      customize =
          do newDebianization' (Just 7) (Just (StandardsVersion 3 9 3 Nothing))
             mapM_ (\ name -> (D.atomSet . debInfo) %= (Set.insert $ D.InstallData (BinPkgName "alex") name name))
                   [ "AlexTemplate"
                   , "AlexTemplate-debug"
                   , "AlexTemplate-ghc"
                   , "AlexTemplate-ghc-debug"
                   , "AlexWrapper-basic"
                   , "AlexWrapper-basic-bytestring"
                   , "AlexWrapper-gscan"
                   , "AlexWrapper-monad"
                   , "AlexWrapper-monad-bytestring"
                   , "AlexWrapper-monadUserState"
                   , "AlexWrapper-monadUserState-bytestring"
                   , "AlexWrapper-posn"
                   , "AlexWrapper-posn-bytestring"
                   , "AlexWrapper-strict-bytestring"]
             (S.homepage . D.control . debInfo) ~= Just "http://www.haskell.org/alex/"
             (D.sourceFormat . debInfo) ~= Just Native3
             (D.debVersion . A.debInfo) ~= Just (parseDebianVersion ("3.0.2-1~hackage1" :: String))
             doExecutable (BinPkgName "alex")
                          (D.InstallFile {D.execName = "alex", D.destName = "alex", D.sourceDir = Nothing, D.destDir = Nothing})
             -- Bootstrap dependency
             (S.buildDepends . D.control . debInfo) %= (++ [[Rel (BinPkgName "alex") Nothing Nothing]])

test10 :: String -> Test
test10 label =
    TestLabel label $
    TestCase (do let inTop = "test-data/archive/input"
                     outTop = "test-data/archive/output"
                 old <- withCurrentDirectory outTop $ newFlags >>= execDebianT inputDebianization . D.makeDebInfo
                 let Just (ChangeLog (entry : _)) = getL D.changelog old
                 new <- withCurrentDirectory inTop $ newFlags >>= newCabalInfo >>= execCabalT (debianize (defaultAtoms >> customize >> (liftCabal $ copyChangelogDate $ logDate entry)))
                 diff <- diffDebianizations old (getL debInfo new)
                 assertEqual label [] diff)
    where
      customize :: CabalT IO ()
      customize =
          do (D.sourceFormat . A.debInfo) ~= Just Native3
             (D.sourcePackageName . A.debInfo) ~= Just (SrcPkgName "seereason-darcs-backups")
             (D.compat . debInfo) ~= Just 5
             (S.standardsVersion . D.control . debInfo) ~= Just (StandardsVersion 3 8 1 Nothing)
             (S.maintainer . D.control . debInfo) ~= either (const Nothing) Just (parseMaintainer "David Fox <dsf@seereason.com>")
             (B.depends . B.relations . D.binaryDebDescription (BinPkgName "seereason-darcs-backups") . debInfo) %= (++ [[Rel (BinPkgName "anacron") Nothing Nothing]])
             (S.section . D.control . debInfo) ~= Just (MainSection "haskell")
             (D.utilsPackageNameBase . A.debInfo) ~= Just "seereason-darcs-backups"
             (D.atomSet . debInfo) %= (Set.insert $ D.InstallCabalExec (BinPkgName "seereason-darcs-backups") "seereason-darcs-backups" "/etc/cron.hourly")

copyChangelogDate :: Monad m => String -> DebianT m ()
copyChangelogDate date =
    D.changelog %= (\ (Just (ChangeLog (entry : older))) -> Just (ChangeLog (entry {logDate = date} : older)))

data Change k a
    = Created k a
    | Deleted k a
    | Modified k a a
    | Unchanged k a
    deriving (Eq, Show)

diffMaps :: (Ord k, Eq a, Show k, Show a) => Map.Map k a -> Map.Map k a -> [Change k a]
diffMaps old new =
    Map.elems (intersectionWithKey combine1 old new) ++
    map (uncurry Deleted) (Map.toList (differenceWithKey combine2 old new)) ++
    map (uncurry Created) (Map.toList (differenceWithKey combine2 new old))
    where
      combine1 k a b = if a == b then Unchanged k a else Modified k a b
      combine2 _ _ _ = Nothing

diffDebianizations :: D.DebInfo -> D.DebInfo -> IO String -- [Change FilePath T.Text]
diffDebianizations old new =
    do old' <- evalDebianT (sortBinaryDebs >> debianizationFileMap) old
       new' <- evalDebianT (sortBinaryDebs >> debianizationFileMap) new
       return $ show $ mconcat $ map prettyChange $ filter (not . isUnchanged) $ diffMaps old' new'
    where
      isUnchanged (Unchanged _ _) = True
      isUnchanged _ = False
      prettyChange :: Change FilePath Text -> Doc
      prettyChange (Unchanged p _) = text "Unchanged: " <> pPrint p <> text "\n"
      prettyChange (Deleted p _) = text "Deleted: " <> pPrint p <> text "\n"
      prettyChange (Created p b) =
          text "Created: " <> pPrint p <> text "\n" <>
          prettyDiff (text ("old" </> p)) (text ("new" </> p)) (text . unpack)
                     -- We use split here instead of lines so we can
                     -- detect whether the file has a final newline
                     -- character.
                     (contextDiff 2 mempty (split (== '\n') b))
      prettyChange (Modified p a b) =
          text "Modified: " <> pPrint p <> text "\n" <>
          prettyDiff (text ("old" </> p)) (text ("new" </> p)) (text . unpack)
                     (contextDiff 2 (split (== '\n') a) (split (== '\n') b))

sortBinaryDebs :: DebianT IO ()
sortBinaryDebs = (S.binaryPackages . D.control) %= sortBy (compare `on` getL B.package)

main :: IO ()
main = runTestTT tests >>= putStrLn . show

