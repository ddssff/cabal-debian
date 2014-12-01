-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( debianization
    , finalizeDebianization' -- external use deprecated - used in test script
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad (when)
import Control.Monad as List (mapM_)
import Control.Monad.State (get, modify)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import Data.Lens.Lazy (access, getL)
import Data.List as List (intercalate, map, nub, unlines)
import Data.Map as Map (delete, elems, lookup, Map, toList, insertWith)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>), mempty)
import Data.Set as Set (difference, filter, fromList, map, null, Set, singleton, toList, union, unions, fold)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (Text, pack, unlines, unpack, intercalate)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.BuildDependencies (debianBuildDeps, debianBuildDepsIndep)
import Debian.Debianize.Changelog (dropFutureEntries)
import Debian.Debianize.DebianName (debianName, debianNameBase)
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms, watchAtom)
import Debian.Debianize.Input (dataDir, inputCabalization, inputChangeLog, inputMaintainer)
import Debian.Debianize.Monad as Monad (DebT)
import Debian.Debianize.Options (compileCommandlineArgs, compileEnvironmentArgs)
import Debian.Debianize.Prelude ((%=), (+=), fromEmpty, fromSingleton, (~=), (~?=))
import qualified Debian.Debianize.Types as T (apacheSite, backups, binaryArchitectures, binaryPackages, binarySection, breaks, buildDepends, buildDependsIndep, buildDir, builtUsing, changelog, comments, compat, conflicts, debianDescription, debVersion, depends, epochMap, executable, extraDevDeps, extraLibMap, file, install, installCabalExec, installData, installDir, installTo, intermediateFiles, link, maintainer, noDocumentationLibrary, noProfilingLibrary, packageDescription, packageType, preDepends, provides, recommends, replaces, revision, rulesFragments, serverInfo, standardsVersion, source, sourceFormat, sourcePackageName, sourcePriority, sourceSection, suggests, utilsPackageNameBase, verbosity, watch, website, control, homepage, official, vcsFields)
import qualified Debian.Debianize.Types.Atoms as A (InstallFile(execName, sourceDir), showAtoms, compilerFlavors, Atom(..), atomSet)
import qualified Debian.Debianize.Types.BinaryDebDescription as B (BinaryDebDescription, package, PackageType(Development, Documentation, Exec, Profiling, Source, HaskellSource, Utilities), PackageType)
import qualified Debian.Debianize.Types.SourceDebDescription as S (xDescription, VersionControlSpec(..))
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Orphans ()
import Debian.Pretty (ppDisplay, PP(..))
import Debian.Policy (getDebhelperCompatLevel, haskellMaintainer, PackageArchitectures(Any, All), PackagePriority(Extra), Section(..), SourceFormat(Quilt3), parseStandardsVersion)
import Debian.Relation (BinPkgName, BinPkgName(BinPkgName), SrcPkgName(SrcPkgName), Relation(Rel), Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..))
import Debian.Release (parseReleaseName)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (buildDebianVersion, DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerFlavor(GHC))
#if MIN_VERSION_Cabal(1,21,0)
import Distribution.Compiler (CompilerFlavor(GHCJS))
#endif
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription as Cabal (allBuildInfo, BuildInfo(buildable, extraLibs), Executable(buildInfo, exeName))
import qualified Distribution.PackageDescription as Cabal (PackageDescription(dataDir, dataFiles, executables, library, package))
import Prelude hiding (init, log, map, unlines, unlines, writeFile, (.))
import System.FilePath ((</>), (<.>), makeRelative, splitFileName, takeDirectory, takeFileName)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianization :: (MonadIO m, Functor m) => DebT m () -> DebT m () -> DebT m ()
debianization init customize =
    do compileEnvironmentArgs
       compileCommandlineArgs
       inputCabalization
       inputChangeLog
       inputMaintainer
       init
       customize
       finalizeDebianization'

-- | Do some light IO and call finalizeDebianization.
finalizeDebianization' :: (MonadIO m, Functor m) => DebT m ()
finalizeDebianization' =
    do date <- liftIO getCurrentLocalRFC822Time
       debhelperCompat <- liftIO getDebhelperCompatLevel
       finalizeDebianization date debhelperCompat
       access T.verbosity >>= \ vb -> when (vb >= 3) (get >>= liftIO . A.showAtoms)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization  :: (MonadIO m, Functor m) => String -> Maybe Int -> DebT m ()
finalizeDebianization date debhelperCompat =
    do -- In reality, hcs must be a singleton or many things won't work.  But some day...
       hcs <- Set.toList <$> access A.compilerFlavors
       finalizeSourceName B.HaskellSource
       List.mapM_ checkOfficialSettings hcs
       List.mapM_ addExtraLibDependencies hcs
       Just pkgDesc <- access T.packageDescription
       T.watch ~?= Just (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       T.sourceSection ~?= Just (MainSection "haskell")
       T.sourcePriority ~?= Just Extra
       T.sourceFormat ~?= Just Quilt3
       T.compat ~?= debhelperCompat
       finalizeChangelog date
       finalizeControl
       -- T.license ~?= Just (Cabal.license pkgDesc)
       expandAtoms
       -- Create the binary packages for the web sites, servers, backup packges, and other executables
       access T.executable >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.backups >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.serverInfo >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access T.website >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       putBuildDeps pkgDesc
       List.mapM_ (librarySpecs pkgDesc) hcs
       List.mapM_ (makeUtilsPackage pkgDesc) hcs
       -- Sketchy - I think more things that need expanded could be generated by the code
       -- executed since the last expandAtoms.  Anyway, should be idempotent.
       expandAtoms
       -- Turn atoms related to priority, section, and description into debianization elements
       -- finalizeDescriptions

-- | Compute the final values of the BinaryDebDescription record
-- description fields from the cabal descriptions and the values that
-- have already been set.
{-
finalizeDescriptions :: (Monad m, Functor m) => DebT m ()
finalizeDescriptions = access T.binaryPackages >>= List.mapM_ finalizeDescription

finalizeDescription :: (Monad m, Functor m) => B.BinaryDebDescription -> DebT m ()
finalizeDescription bdd =
    do let b = getL B.package bdd
       cabDesc <- describe
       T.debianDescription ~?= Just cabDesc
-}

-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
debianVersion :: Monad m => DebT m DebianVersion
debianVersion =
    do pkgDesc <- access T.packageDescription >>= maybe (error "debianVersion: no PackageDescription") return
       let pkgId = Cabal.package pkgDesc
       epoch <- debianEpoch (pkgName pkgId)
       debVer <- access T.debVersion
       case debVer of
         Just override
             | override < parseDebianVersion (ppDisplay (pkgVersion pkgId)) ->
                 error ("Version from --deb-version (" ++ ppDisplay override ++
                        ") is older than hackage version (" ++ ppDisplay (pkgVersion pkgId) ++
                        "), maybe you need to unpin this package?")
         Just override -> return override
         Nothing ->
             do let ver = ppDisplay (pkgVersion pkgId)
                rev <- access T.revision
                let revMB = case rev of Nothing -> Nothing
                                        Just "" -> Nothing
                                        Just "-" -> Nothing
                                        Just ('-':r) -> Just r
                                        Just _ -> error "The Debian revision needs to start with a dash"
                return $ buildDebianVersion epoch ver revMB

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> DebT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . getL T.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
finalizeSourceName :: (Monad m, Functor m) => B.PackageType -> DebT m ()
finalizeSourceName typ =
    do DebBase debName <- debianNameBase
       T.sourcePackageName ~?= Just (SrcPkgName (case typ of
                                                   B.HaskellSource -> "haskell-" ++ debName
                                                   B.Source -> debName
                                                   _ -> error $ "finalizeSourceName: " ++ show typ))

finalizeMaintainer :: Monad m => DebT m ()
finalizeMaintainer =
    T.maintainer ~?= Just haskellMaintainer

finalizeControl :: (Monad m, Functor m) => DebT m ()
finalizeControl =
    do finalizeMaintainer
       Just src <- access T.sourcePackageName
       maint <- access T.maintainer >>= return . fromMaybe (error "No maintainer")
       T.source ~= Just src
       T.maintainer ~= Just maint
       desc <- describe
       (S.xDescription . T.control) ~?= Just desc
       -- control %= (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: (Monad m, Functor m) => String -> DebT m ()
finalizeChangelog date =
    do finalizeMaintainer
       ver <- debianVersion
       src <- access T.sourcePackageName
       Just maint <- access T.maintainer
       cmts <- access T.comments
       T.changelog %= fmap (dropFutureEntries ver)
       T.changelog %= fixLog src ver cmts maint
    where
      -- Ensure that the package name is correct in the first log entry.
      fixLog src ver cmts _maint (Just (ChangeLog (entry : older))) | logVersion entry == ver =
          Just (ChangeLog (entry { logPackage = show (pPrint (PP src))
                                 , logComments = logComments entry ++ "\n" ++
                                                 (List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack) (fromMaybe [] cmts))
                                 } : older))
      -- The newest log entry isn't exactly ver, build a new entry.
      fixLog src ver cmts maint log =
          Just (ChangeLog (Entry { logPackage = show (pPrint (PP src))
                                 , logVersion = ver
                                 , logDists = [parseReleaseName "UNRELEASED"]
                                 , logUrgency = "low"
                                 , logComments = List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack)
                                                 (fromMaybe [["Debianization generated by cabal-debian"]] cmts)
                                 , logWho = ppDisplay maint
                                 , logDate = date } : maybe [] (\ (ChangeLog entries) -> entries) log))

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: (Monad m, Functor m) => CompilerFlavor -> DebT m ()
addExtraLibDependencies hc =
    do pkgDesc <- access T.packageDescription >>= maybe (error "addExtraLibDependencies: no PackageDescription") return
       devName <- debianName B.Development hc
       libMap <- access T.extraLibMap
       binNames <- List.map (getL B.package) <$> access T.binaryPackages
       when (any (== devName) binNames) (T.depends devName %= \ deps -> deps ++ g pkgDesc libMap)
    where
      g :: PackageDescription -> Map String Relations -> Relations
      g pkgDesc libMap = concatMap (devDep libMap) (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)
      devDep :: Map String Relations -> String -> Relations
      devDep libMap cab = maybe [[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]] id (Map.lookup cab libMap)

-- | Applies a few settings to official packages (unless already set)
checkOfficialSettings :: (Monad m, Functor m) => CompilerFlavor -> DebT m ()
checkOfficialSettings flavor =
    do o <- access T.official
       when o $ case flavor of
                  GHC -> officialSettings
                  _ -> error $ "There is no official packaging for " ++ show flavor

officialSettings :: (Monad m, Functor m) => DebT m ()
officialSettings =
    do pkgDesc <- access T.packageDescription >>= maybe (error "officialSettings: no PackageDescription") return
       let PackageName cabal = pkgName (Cabal.package pkgDesc)

       T.standardsVersion ~?= Just (parseStandardsVersion "3.9.5")
       T.homepage ~?= Just ("http://hackage.haskell.org/package/" <> pack cabal)
       SrcPkgName src <- access T.sourcePackageName >>= maybe (error "officialSettings: no sourcePackageName") return

       T.vcsFields %= Set.union (Set.fromList
          [ S.VCSBrowser $ "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/" <> pack src
          , S.VCSDarcs  $ "http://darcs.debian.org/pkg-haskell/" <> pack src
          ])
 

putBuildDeps :: (MonadIO m, Functor m) => PackageDescription -> DebT m ()
putBuildDeps pkgDesc =
    do deps <- debianBuildDeps pkgDesc
       depsIndep <- debianBuildDepsIndep pkgDesc
       T.buildDepends ~= deps
       T.buildDependsIndep ~= depsIndep

cabalExecBinaryPackage :: Monad m => BinPkgName -> DebT m ()
cabalExecBinaryPackage b =
    do T.packageType b ~?= Just B.Exec
       T.binaryArchitectures b ~?= Just Any
       T.binarySection b ~?= Just (MainSection "misc")
       T.debianDescription b ~?= Just desc -- yeah, this same line is all over the place.
       binaryPackageRelations b B.Exec
    where

binaryPackageRelations :: Monad m => BinPkgName -> B.PackageType -> DebT m ()
binaryPackageRelations b typ =
    do edds <- access T.extraDevDeps
       T.depends b %= \ rels ->
          [anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
          [anyrel "${shlibs:Depends}" | typ `notElem` [B.Profiling, B.Documentation] ] ++
          edds ++ rels
       T.recommends b %= \ rels -> [anyrel "${haskell:Recommends}"] ++ rels
       T.suggests b %= \ rels -> [anyrel "${haskell:Suggests}"] ++ rels
       T.preDepends b ~= []
       T.breaks b ~= []
       T.conflicts b %= \ rels -> [anyrel "${haskell:Conflicts}"] ++ rels
       T.provides b %= \ rels -> (if typ /= B.Documentation then [anyrel "${haskell:Provides}"] else []) ++ rels
       -- T.replaces b %= \ rels -> [anyrel "${haskell:Replaces}"] ++ rels
       T.builtUsing b ~= []

-- | Add the library paragraphs for a particular compiler flavor.
librarySpecs :: (Monad m, Functor m) => PackageDescription -> CompilerFlavor -> DebT m ()
librarySpecs pkgDesc hc =
    do let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . getL T.noDocumentationLibrary
       prof <- get >>= return . not . getL T.noProfilingLibrary
       when dev (librarySpec Any B.Development hc)
       when (dev && prof && hc == GHC) (librarySpec Any B.Profiling hc)
       when (dev && doc) (docSpecsParagraph hc)

docSpecsParagraph :: (Monad m, Functor m) => CompilerFlavor -> DebT m ()
docSpecsParagraph hc =
    do b <- debianName B.Documentation hc
       binaryPackageRelations b B.Documentation
       T.packageType b ~?= Just B.Documentation
       T.packageType b ~?= Just B.Documentation
       T.binaryArchitectures b ~= Just All
       T.binarySection b ~?= Just (MainSection "doc")
       T.debianDescription b ~?= Just desc

librarySpec :: (Monad m, Functor m) => PackageArchitectures -> B.PackageType -> CompilerFlavor -> DebT m ()
librarySpec arch typ hc =
    do b <- debianName typ hc
       binaryPackageRelations b typ
       T.packageType b ~?= Just typ
       T.packageType b ~?= Just typ
       T.binaryArchitectures b ~?= Just arch
       T.debianDescription b ~?= Just desc

-- | This is the standard value for the Description field of a binary
-- package control file stanza.
desc :: Text
desc = Text.intercalate "\n "
         ["${haskell:ShortDescription}${haskell:ShortBlurb}",
          " ${haskell:LongDescription}",
          " .",
          " ${haskell:Blurb}"]

-- | Make sure all data and executable files are assigned to at least
-- one binary package and make sure all binary packages are in the
-- package list in the source deb description.  If there are left over
-- files, assign them to the packages returned by the
-- utilsPackageNames lens, and make sure those packages are in the
-- source deb description.
makeUtilsPackage :: forall m. (MonadIO m, Functor m) => PackageDescription -> CompilerFlavor -> DebT m ()
makeUtilsPackage pkgDesc hc =
    do -- Files the cabal package expects to be installed
       -- Files that are already assigned to any binary deb
       installedDataMap <- Set.fold (\ x r ->
                                         case x of
                                           A.Install b from _ -> Map.insertWith Set.union b (singleton from) r
                                           A.InstallTo b from _ -> Map.insertWith Set.union b (singleton from) r
                                           A.InstallData b from _ -> Map.insertWith Set.union b (singleton from) r
                                           _ -> r) mempty <$> access A.atomSet :: DebT m (Map BinPkgName (Set FilePath))
       installedExecMap <- Set.fold (\ x r ->
                                         case x of
                                           A.InstallCabalExec b name _ -> Map.insertWith Set.union b (singleton name) r
                                           A.InstallCabalExecTo b name _ -> Map.insertWith Set.union b (singleton name) r
                                           _ -> r) mempty <$> access A.atomSet :: DebT m (Map BinPkgName (Set String))

       -- The names of cabal executables that go into eponymous debs
       insExecPkg <- access T.executable >>= return . Set.map ename . Set.fromList . elems

       let installedData = Set.map (\ a -> (a, a)) $ Set.unions (Map.elems installedDataMap)
           installedExec = Set.unions (Map.elems installedExecMap)

       let prefixPath = Cabal.dataDir pkgDesc
       let dataFilePaths = Set.fromList (zip (List.map (prefixPath </>) (Cabal.dataFiles pkgDesc)) (Cabal.dataFiles pkgDesc)) :: Set (FilePath, FilePath)
           execFilePaths = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc))) :: Set FilePath
       let availableData = Set.union installedData dataFilePaths
           availableExec = Set.union installedExec execFilePaths

       access T.utilsPackageNameBase >>= \ name ->
           case name of
             Nothing -> debianName B.Utilities hc >>= \ (BinPkgName name') -> T.utilsPackageNameBase ~= Just name'
             _ -> return ()
       b <- debianName B.Utilities hc

       -- Files that are installed into packages other than the utils packages
       let installedDataOther = Set.map (\ a -> (a, a)) $ Set.unions $ Map.elems $ Map.delete b installedDataMap
           installedExecOther = Set.union insExecPkg $ Set.unions $ Map.elems $ Map.delete b installedExecMap

       -- Files that will be in utils packages
       let utilsData = Set.difference availableData installedDataOther
           utilsExec = Set.difference availableExec installedExecOther
       -- Files that still need to be assigned to the utils packages
       let utilsDataMissing = Set.difference utilsData installedData
           utilsExecMissing = Set.difference utilsExec installedExec
       -- If any files belong in the utils packages, make sure they exist
       when (not (Set.null utilsData && Set.null utilsExec)) $ do
         T.debianDescription b ~?= Just desc
         -- This is really for all binary debs except the libraries - I'm not sure why
         T.rulesFragments += (pack ("build" </> ppDisplay b ++ ":: build-ghc-stamp\n"))
         T.binaryArchitectures b ~?= Just (if Set.null utilsExec then All else Any)
         T.binarySection b ~?= Just (MainSection "misc")
         binaryPackageRelations b B.Utilities
       -- Add the unassigned files to the utils packages
       Set.mapM_ (uncurry (T.installData b)) utilsDataMissing
       Set.mapM_ (\ name -> T.installCabalExec b name "usr/bin") utilsExecMissing
    where
      ename i =
          case A.sourceDir i of
            (Nothing) -> A.execName i
            (Just s) ->  s </> A.execName i

expandAtoms :: Monad m => DebT m ()
expandAtoms =
    do hcs <- access A.compilerFlavors >>= return . Set.toList
       builddir <- access T.buildDir >>= return . fromEmpty (case hcs of
                                                               [GHC] -> singleton "dist-ghc/build"
#if MIN_VERSION_Cabal(1,21,0)
                                                               [GHCJS] -> singleton "dist-ghcjs/build"
#endif
                                                               _ -> error $ "Unexpected compiler: " ++ show hcs)
       dDir <- access T.packageDescription >>= maybe (error "expandAtoms") (return . dataDir)
       expandApacheSites
       expandInstallCabalExecs (fromSingleton (error "no builddir") (\ xs -> error $ "multiple builddirs:" ++ show xs) builddir)
       expandInstallCabalExecTo (fromSingleton (error "no builddir") (\ xs -> error $ "multiple builddirs:" ++ show xs) builddir)
       expandInstallData dDir
       expandInstallTo
       expandFile
       expandWebsite
       expandServer
       expandBackups
       expandExecutable
    where
      expandApacheSites :: Monad m => DebT m ()
      expandApacheSites =
          do mp <- get >>= return . getL T.apacheSite
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do T.link b ("/etc/apache2/sites-available/" ++ dom) ("/etc/apache2/sites-enabled/" ++ dom)
                   T.installDir b log
                   T.file b ("/etc/apache2/sites-available" </> dom) text

      -- Turn A.InstallCabalExec into A.Install
      expandInstallCabalExecs :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecs builddir = do
        hcs <- access A.compilerFlavors >>= return . Set.toList
        access A.atomSet >>= List.mapM_ (doAtom hcs) . Set.toList
          where
            doAtom [GHC] (A.InstallCabalExec b name dest) = T.install b (builddir </> name </> name) dest
            -- A GHCJS executable is a directory with files, copy them
            -- all into place.
#if MIN_VERSION_Cabal(1,21,0)
            doAtom [GHCJS] (A.InstallCabalExec b name dest) =
                T.rulesFragments +=
                     (Text.unlines
                        [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                        , pack ("\t(cd " <> builddir </> name <> " && find " <> name <.> "jsexe" <> " -type f) |\\\n" <>
                                       "\t  while read i; do install -Dp " <> builddir </> name </> "$$i debian" </> ppDisplay b </> makeRelative "/" dest </> "$$i; done\n") ])
#endif
            doAtom _ _ = return ()

      -- Turn A.InstallCabalExecTo into a make rule
      expandInstallCabalExecTo :: Monad m => FilePath -> DebT m ()
      expandInstallCabalExecTo builddir = do
        hcs <- access A.compilerFlavors >>= return . Set.toList
        access A.atomSet >>= List.mapM_ (doAtom hcs) . Set.toList
          where
            doAtom [GHC] (A.InstallCabalExecTo b name dest) =
                T.rulesFragments += (Text.unlines
                                       [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                                       , "\tinstall -Dps " <> pack (builddir </> name </> name) <> " "
                                                           <> pack ("debian" </> ppDisplay b </> makeRelative "/" dest) ])
            doAtom hcs (A.InstallCabalExecTo b name dest) = error $ "expandInstallCabalExecTo " ++ show hcs ++ " " ++ show (A.InstallCabalExecTo b name dest)
            doAtom _ _ = return ()

      -- Turn A.InstallData into either an Install or an InstallTo
      expandInstallData :: Monad m => FilePath -> DebT m ()
      expandInstallData dDir =
          access A.atomSet >>= List.mapM_ doAtom . Set.toList
          where
            doAtom (A.InstallData b from dest) =
                if takeFileName from == takeFileName dest
                then T.install b from (dDir </> makeRelative "/" (takeDirectory dest))
                else T.installTo b from (dDir </> makeRelative "/" dest)
            doAtom _ = return ()

      -- Turn A.InstallTo into a make rule
      expandInstallTo :: Monad m => DebT m ()
      expandInstallTo =
          access A.atomSet >>= List.mapM_ doAtom . Set.toList
          where
            doAtom (A.InstallTo b from dest) =
                T.rulesFragments += (Text.unlines [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                                                  , "\tinstall -Dp " <> pack from <> " " <> pack ("debian" </> ppDisplay b </> makeRelative "/" dest) ])
            doAtom _ = return ()

      -- Turn A.File into an intermediateFile and an A.Install
      expandFile :: Monad m => DebT m ()
      expandFile =
          access A.atomSet >>= List.mapM_ doAtom . Set.toList
          where
            doAtom (A.File b path text) =
                do let (destDir', destName') = splitFileName path
                       tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack text)))
                       tmpPath = tmpDir </> destName'
                   T.intermediateFiles += (tmpPath, text)
                   T.install b tmpPath destDir'
            doAtom _ = return ()

      expandWebsite :: Monad m => DebT m ()
      expandWebsite =
          do mp <- get >>= return . getL T.website
             List.mapM_ (\ (b, site) -> modify (siteAtoms b site)) (Map.toList mp)

      expandServer :: Monad m => DebT m ()
      expandServer =
          do mp <- get >>= return . getL T.serverInfo
             List.mapM_ (\ (b, x) -> modify (serverAtoms b x False)) (Map.toList mp)

      expandBackups :: Monad m => DebT m ()
      expandBackups =
          do mp <- get >>= return . getL T.backups
             List.mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)

      expandExecutable :: Monad m => DebT m ()
      expandExecutable =
          do mp <- get >>= return . getL T.executable
             List.mapM_ (\ (b, f) -> modify (execAtoms b f)) (Map.toList mp)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]
