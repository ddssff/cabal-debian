-- | [/QUICK START:/]
--
-- You can either run the @cabal-debian --debianize@, or
-- for more power and flexibility you can put a @Debianize.hs@ script in
-- the package's @debian@ subdirectory.
-- 'Debian.Debianize.CabalInfo' value and pass it to the
-- 'Debian.Debianize.debianize' function.  The
-- 'Debian.Debianize.callDebianize' function retrieves extra arguments
-- from the @CABALDEBIAN@ environment variable and calls
-- 'Debian.Debianize.debianize' with the build directory set as it
-- would be when the packages is built by @dpkg-buildpackage@.
--
-- To see what your debianization would produce, or how it differs
-- from the debianization already present:
--
-- > % cabal-debian --debianize -n
--
-- This is equivalent to the library call
--
-- > % ghc -e 'Debian.Debianize.callDebianize ["-n"]'
--
-- To actually create the debianization and then build the debs,
--
-- > % ghc -e 'Debian.Debianize.callDebianize []'
-- > % sudo dpkg-buildpackage
--
-- At this point you may need to modify Cabal.defaultFlags to achieve
-- specific packaging goals.  Create a module for this in debian/Debianize.hs:
--
-- > import Data.Lens.Lazy
-- > import Data.Map as Map (insertWith)
-- > import Data.Set as Set (union, singleton)
-- > import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
-- > import Debian.Debianize (defaultAtoms, depends, debianization, writeDebianization)
-- > main = debianization "." defaultAtoms >>=
-- >        return . modL depends (insertWith union (BinPkgName "cabal-debian") (singleton (Rel (BinPkgName "debian-policy") Nothing Nothing))) >>=
-- >        writeDebianization "."
--
-- Then to test it,
--
-- > % CABALDEBIAN='["-n"]' runhaskell debian/Debianize.hs
--
-- or equivalently
--
-- > % ghc -e 'Debian.Debianize.runDebianize ["-n"]'
--
-- and to run it for real:
--
-- > % runhaskell debian/Debianize.hs
--
-- [/DESIGN OVERVIEW/]
--
-- The three phases of the operation of the system are Input -> Finalization -> Output.
--
--    [Input] Module "Debian.Debianize.Input" - gather inputs using IO
--    operations and customization functions, from the .cabal file, an
--    existing debianization, and so on.  This information results in
--    a value of type @Atoms@.  Modules @Types@, @Lenses@, @Inputs@.
--
--    [Customize] Make modifications to the input values
--
--    [Finalization] Module "Debian.Debianize.Finalize" - Fill in any
--    information missing from @Atoms@ that is required to build the
--    debianization based on the inputs and our policy decisions.
--
--    [Debianize] Module "Debian.Debianize.Files" - Compute the paths
--    and files of the debianization from the Atoms value.
--
--    [Output] Module "Debian.Debianize.Output" - Perform a variety of
--    output operations on the debianzation - writing or updating the
--    files in a debian directory, comparing two debianizations,
--    validate a debianization (ensure two debianizations match in
--    source and binary package names), or describe a debianization.
--
-- There is also a high level function to run a script that runs this
-- entire pipeline when it finds from a script found in a
-- debian/Debianize.hs file.

{-
   Types.hs
   Monad.hs
   Lenses.hs

   Input.hs

 * Files.hs

   Output.hs

   Bundled.hs
   Changelog.hs
   DebianName.hs
   Details.hs
   Finalize.hs
   Goodies.hs
   Interspersed.hs
   Options.hs
   SubstVars.hs
   Tests.hs
   Utility.hs
   VersionSplits.hs
-}
module Debian.Debianize
    ( -- * Collect information about desired debianization
      module Debian.Debianize.BasicInfo
    , module Debian.Debianize.DebInfo
    , module Debian.Debianize.SourceDebDescription
    , module Debian.Debianize.BinaryDebDescription
    , module Debian.Debianize.CopyrightDescription
    , module Debian.Debianize.CabalInfo
      -- * State monads to carry the collected information, command line options
    , module Debian.Debianize.Monad
    , module Debian.Debianize.Options
      -- * Functions for maping Cabal name and version number to Debian name
    , module Debian.Debianize.DebianName
      -- * Specific details about the particular packages and versions in the Debian repo
    , module Debian.Debianize.Details
      -- * Functions to configure some useful packaging idioms - web server packages,
      -- tight install dependencies, etc.
    , module Debian.Debianize.Goodies
      -- * IO functions for reading debian or cabal packaging info
    , module Debian.Debianize.InputDebian
    , module Debian.Debianize.InputCabal
      -- * Finish computing the debianization and output the result
    , module Debian.Debianize.Finalize
    , module Debian.Debianize.Output
      -- * Utility functions
    , module Debian.Debianize.Prelude
    , module Debian.Debianize.VersionSplits
    , module Debian.Policy
{-
    , Debian.Debianize.Finalize.debianize
    , Debian.Debianize.Finalize.finalizeDebianization

    , Debian.Debianize.Output.doDebianizeAction
    , Debian.Debianize.Output.runDebianizeScript
    , Debian.Debianize.Output.writeDebianization
    , Debian.Debianize.Output.describeDebianization
    , Debian.Debianize.Output.compareDebianization
    , Debian.Debianize.Output.validateDebianization

    , Debian.Debianize.Details.debianDefaultAtoms

    , Debian.Debianize.Goodies.tightDependencyFixup
    , Debian.Debianize.Goodies.doExecutable
    , Debian.Debianize.Goodies.doServer
    , Debian.Debianize.Goodies.doWebsite
    , Debian.Debianize.Goodies.doBackups

    , Debian.Debianize.InputDebian.inputDebianization
    , Debian.Debianize.InputDebian.inputDebianizationFile
    , Debian.Debianize.InputDebian.inputChangeLog

    , Debian.Debianize.DebianName.mapCabal
    , Debian.Debianize.DebianName.splitCabal
    , Debian.Debianize.Options.compileArgs
    , Debian.Debianize.SubstVars.substvars

    -- * Utility functions

    , Debian.Debianize.Prelude.withCurrentDirectory
    , Debian.Debianize.Prelude.buildDebVersionMap
    , Debian.Debianize.Prelude.dpkgFileMap
    , Debian.Debianize.Prelude.debOfFile
    -- * Lens operators
    , (~=)
    , (~?=)
    , (%=)
    , (+=)
    , (++=)
    , (+++=)

-}
    ) where

import Debian.Debianize.CabalInfo -- (debianNameMap, debInfo, epochMap, newAtoms, packageDescription, PackageInfo, packageInfo, showAtoms)
import Debian.Debianize.BasicInfo
import Debian.Debianize.BinaryDebDescription
import Debian.Debianize.CopyrightDescription
import Debian.Debianize.DebInfo -- (Atom(..), atomSet, changelog, compat, control, copyright, DebInfo, file, flags, install, installCabalExec, installCabalExecTo, installData, installDir, installInit, installTo, intermediateFiles, link, logrotateStanza, makeDebInfo, postInst, postRm, preInst, preRm, rulesFragments, rulesHead, rulesIncludes, rulesSettings, sourceFormat, warning, watch, apacheSite, backups, buildDir, comments, debVersion, execMap, executable, extraDevDeps, extraLibMap, InstallFile(..), maintainerOption, missingDependencies, noDocumentationLibrary, noProfilingLibrary, official, omitLTDeps, omitProfVersionDeps, revision, Server(..), serverInfo, Site(..), sourceArchitectures, sourcePackageName, uploadersOption, utilsPackageNameBase, website, xDescription, overrideDebianNameBase)
import Debian.Debianize.DebianName (mapCabal, splitCabal, remapCabal)
import Debian.Debianize.Details (debianDefaults)
import Debian.Debianize.Finalize (debianize)
import Debian.Debianize.Goodies -- (doBackups, doExecutable, doServer, doWebsite, tightDependencyFixup)
import Debian.Debianize.InputDebian (inputChangeLog, inputDebianization, inputDebianizationFile)
import Debian.Debianize.InputCabal (inputCabalization)
import Debian.Debianize.Monad (CabalM, CabalT, evalCabalM, evalCabalT, execCabalM, execCabalT, runCabalM, runCabalT, DebianT, execDebianT, evalDebianT, liftCabal)
import Debian.Debianize.Options (compileArgs)
import Debian.Debianize.Output (compareDebianization, describeDebianization, doDebianizeAction, runDebianizeScript, validateDebianization, writeDebianization)
import Debian.Debianize.Prelude ((%=), (+++=), (++=), (+=), buildDebVersionMap, debOfFile, dpkgFileMap, withCurrentDirectory, (~=), (~?=))
import Debian.Debianize.SourceDebDescription
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Policy (accessLogBaseName, apacheAccessLog, apacheErrorLog, apacheLogDirectory, appLogBaseName, Area(..), databaseDirectory, debianPackageVersion, errorLogBaseName, fromCabalLicense, getCurrentDebianUser, getDebhelperCompatLevel, getDebianStandardsVersion, haskellMaintainer, License(..), PackageArchitectures(..), PackagePriority(..), parseMaintainer, parsePackageArchitectures, parseStandardsVersion, parseUploaders, readLicense, readPriority, readSection, readSourceFormat, Section(..), serverAccessLog, serverAppLog, serverLogDirectory, SourceFormat(..), StandardsVersion(..), toCabalLicense)
