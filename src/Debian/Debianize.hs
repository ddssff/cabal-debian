-- | [/QUICK START:/]
--
-- You can either run the @cabal-debian --debianize@, or
-- for more power and flexibility you can put a @Debianize.hs@ script in
-- the package's @debian@ subdirectory.
-- 'Debian.Debianize.Atoms' value and pass it to the
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
    ( Debian.Debianize.Finalize.debianize
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

    , Debian.Debianize.Input.inputDebianization
    , Debian.Debianize.Input.inputDebianizationFile
    , Debian.Debianize.Input.inputChangeLog

    -- * Deb monad - 'Debian.Debianize.Monad'
    , CabalT, runCabalT, execCabalT, evalCabalT, CabalM, runCabalM, execCabalM, evalCabalM

    , Debian.Debianize.DebianName.mapCabal
    , Debian.Debianize.DebianName.splitCabal
    , Debian.Debianize.Options.compileArgs
    , Debian.Debianize.SubstVars.substvars

    -- * Utility functions

    , Debian.Debianize.Prelude.withCurrentDirectory
    , Debian.Debianize.Prelude.buildDebVersionMap
    , Debian.Debianize.Prelude.dpkgFileMap
    , Debian.Debianize.Prelude.debOfFile
    , (~=)
    , (~?=)
    , (%=)
    , (+=)
    , (++=)
    , (+++=)

    -- * TBD

    , module Debian.Debianize.Types
    , module Debian.Debianize.DebInfo
    , module Debian.Debianize.Types.Atoms
    , module Debian.Policy
    ) where

import Debian.Debianize.DebInfo (Atom(..), atomSet, changelog, compat, control, copyright, DebInfo(..), file, flags, install, installCabalExec, installCabalExecTo, installData, installDir, installInit, installTo, intermediateFiles, link, logrotateStanza, makeDebInfo, postInst, postRm, preInst, preRm, rulesFragments, rulesHead, rulesIncludes, rulesSettings, sourceFormat, warning, watch)
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Finalize (debianize, finalizeDebianization)
import Debian.Debianize.Goodies (doBackups, doExecutable, doServer, doWebsite, tightDependencyFixup)
import Debian.Debianize.Input (inputChangeLog, inputDebianization, inputDebianizationFile)
import Debian.Debianize.Monad (Atoms, CabalM, CabalT, evalCabalM, evalCabalT, execCabalM, execCabalT, runCabalM, runCabalT)
import Debian.Debianize.Options (compileArgs)
import Debian.Debianize.Output (compareDebianization, describeDebianization, doDebianizeAction, runDebianizeScript, validateDebianization, writeDebianization)
import Debian.Debianize.Prelude ((%=), (+++=), (++=), (+=), buildDebVersionMap, debOfFile, dpkgFileMap, withCurrentDirectory, (~=), (~?=))
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types (apacheSite, backups, binaryArchitectures, binaryPackages, binaryPriority, binarySection, breaks, buildConflicts, buildConflictsIndep, buildDepends, buildDependsIndep, buildDir, builtUsing, changedBy, changelog, comments, compat, conflicts, control, copyright, debianDescription, debianMaintainer, debianNameMap, debianUploaders, debVersion, depends, dmUploadAllowed, epochMap, essential, execMap, executable, extraDevDeps, extraLibMap, file, flags, homepage, install, installCabalExec, installCabalExecTo, installData, installDir, installInit, installTo, intermediateFiles, link, logrotateStanza, maintainerOption, missingDependencies, noDocumentationLibrary, noProfilingLibrary, official, omitLTDeps, omitProfVersionDeps, overrideDebianNameBase, packageDescription, packageInfo, packageType, postInst, postRm, preDepends, preInst, preRm, provides, recommends, replaces, revision, rulesFragments, rulesHead, rulesIncludes, rulesSettings, serverInfo, source, sourceArchitectures, sourceFormat, sourcePackageName, sourcePriority, sourceSection, standardsVersion, suggests, uploaders, uploadersOption, utilsPackageNameBase, vcsFields, warning, watch, website, xDescription, xFields)
import Debian.Debianize.Types.Atoms (apacheSite, Atoms(..), backups, buildDir, comments, debianNameMap, debInfo, debVersion, epochMap, execMap, executable, extraDevDeps, extraLibMap, InstallFile(..), maintainerOption, makeAtoms, missingDependencies, newAtoms, noDocumentationLibrary, noProfilingLibrary, official, omitLTDeps, omitProfVersionDeps, overrideDebianNameBase, packageDescription, packageInfo, PackageInfo(..), revision, Server(..), serverInfo, showAtoms, Site(..), sourceArchitectures, sourcePackageName, uploadersOption, utilsPackageNameBase, website, xDescription)
import Debian.Policy (accessLogBaseName, apacheAccessLog, apacheErrorLog, apacheLogDirectory, appLogBaseName, Area(..), databaseDirectory, debianPackageVersion, errorLogBaseName, fromCabalLicense, getCurrentDebianUser, getDebhelperCompatLevel, getDebianStandardsVersion, haskellMaintainer, License(..), PackageArchitectures(..), PackagePriority(..), parseMaintainer, parsePackageArchitectures, parseStandardsVersion, parseUploaders, readLicense, readPriority, readSection, readSourceFormat, Section(..), serverAccessLog, serverAppLog, serverLogDirectory, SourceFormat(..), StandardsVersion(..), toCabalLicense)
