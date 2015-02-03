-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( debianize
    , finalizeDebianization -- external use deprecated - used in test script
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad (unless, when)
import Control.Monad as List (mapM_)
import Control.Monad.State (get, modify)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Lens.Lazy (access, getL)
import Data.List as List (intercalate, map, nub, null, unlines)
import Data.Map as Map (delete, elems, insertWith, lookup, Map, toList)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Set as Set (difference, filter, fold, fromList, insert, map, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (intercalate, pack, Text, unlines, unpack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.BasicInfo (cabalFlagAssignments, compilerFlavor, verbosity)
import Debian.Debianize.BuildDependencies (debianBuildDeps, debianBuildDepsIndep)
import Debian.Debianize.Changelog (dropFutureEntries)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.DebianName (debianName, debianNameBase)
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms, watchAtom)
import Debian.Debianize.InputDebian (dataDir, inputChangeLog)
import Debian.Debianize.Monad as Monad (CabalT, liftCabal)
import Debian.Debianize.Options (compileCommandlineArgs, compileEnvironmentArgs)
import Debian.Debianize.Prelude ((%=), (+=), (~=), (~?=))
import qualified Debian.Debianize.CabalInfo as A
import qualified Debian.Debianize.BinaryDebDescription as B
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Orphans ()
import Debian.Policy (getCurrentDebianUser, getDebhelperCompatLevel, haskellMaintainer, PackageArchitectures(Any, All), PackagePriority(Extra), parseMaintainer, parseStandardsVersion, Section(..), SourceFormat(Native3, Quilt3))
import Debian.Pretty (PP(..), ppDisplay)
import Debian.Relation (BinPkgName, BinPkgName(BinPkgName), Relation(Rel), Relations, SrcPkgName(SrcPkgName))
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..))
import Debian.Release (parseReleaseName)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (buildDebianVersion, DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerFlavor(GHC), CompilerFlavor(GHCJS))
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (FlagName(FlagName), PackageDescription)
import Distribution.PackageDescription as Cabal (allBuildInfo, author, BuildInfo(buildable, extraLibs), Executable(buildInfo, exeName), maintainer)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(dataDir, dataFiles, executables, library, package))
import Prelude hiding ((.), init, log, map, unlines, unlines, writeFile)
import System.FilePath ((<.>), (</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianize :: (MonadIO m, Functor m) => CabalT m () -> CabalT m ()
debianize customize =
    do compileEnvironmentArgs
       compileCommandlineArgs
       liftCabal inputChangeLog
       customize
       finalizeDebianization

-- | Do some light IO and call finalizeDebianization.
finalizeDebianization :: (MonadIO m, Functor m) => CabalT m ()
finalizeDebianization =
    do date <- liftIO getCurrentLocalRFC822Time
       debhelperCompat <- liftIO getDebhelperCompatLevel
       finalizeDebianization' date debhelperCompat
       access (verbosity . D.flags . A.debInfo) >>= \ vb -> when (vb >= 3) (get >>= liftIO . A.showCabalInfo)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization'  :: (MonadIO m, Functor m) => String -> Maybe Int -> CabalT m ()
finalizeDebianization' date debhelperCompat =
    do -- In reality, hcs must be a singleton or many things won't work.  But some day...
       hc <- access (compilerFlavor . D.flags . A.debInfo)
       finalizeSourceName B.HaskellSource
       checkOfficialSettings hc
       addExtraLibDependencies hc
       pkgDesc <- access A.packageDescription
       (D.watch . A.debInfo) ~?= Just (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       (S.section . D.control . A.debInfo) ~?= Just (MainSection "haskell")
       (S.priority . D.control . A.debInfo) ~?= Just Extra
       (D.sourceFormat . A.debInfo) ~?= Just Quilt3
       (D.compat . A.debInfo) ~?= debhelperCompat
       finalizeChangelog date
       finalizeControl
       finalizeRules
       -- T.license ~?= Just (Cabal.license pkgDesc)
       expandAtoms
       -- Create the binary packages for the web sites, servers, backup packges, and other executables
       access (D.executable . A.debInfo) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access (D.backups . A.debInfo) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access (D.serverInfo . A.debInfo) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       access (D.website . A.debInfo) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       putBuildDeps pkgDesc
       librarySpecs pkgDesc hc
       makeUtilsPackage pkgDesc hc
       -- Sketchy - I think more things that need expanded could be generated by the code
       -- executed since the last expandAtoms.  Anyway, should be idempotent.
       expandAtoms
       -- Turn atoms related to priority, section, and description into debianization elements
       -- finalizeDescriptions

-- | Compute the final values of the BinaryDebDescription record
-- description fields from the cabal descriptions and the values that
-- have already been set.
{-
finalizeDescriptions :: (Monad m, Functor m) => CabalT m ()
finalizeDescriptions = access T.binaryPackages >>= List.mapM_ finalizeDescription

finalizeDescription :: (Monad m, Functor m) => B.BinaryDebDescription -> CabalT m ()
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
debianVersion :: Monad m => CabalT m DebianVersion
debianVersion =
    do pkgDesc <- access A.packageDescription
       let pkgId = Cabal.package pkgDesc
       epoch <- debianEpoch (pkgName pkgId)
       debVer <- access (D.debVersion . A.debInfo)
       case debVer of
         Just override
             | override < parseDebianVersion (ppDisplay (pkgVersion pkgId)) ->
                 error ("Version from --deb-version (" ++ ppDisplay override ++
                        ") is older than hackage version (" ++ ppDisplay (pkgVersion pkgId) ++
                        "), maybe you need to unpin this package?")
         Just override -> return override
         Nothing ->
             do let ver = ppDisplay (pkgVersion pkgId)
                rev <- access (D.revision . A.debInfo)
                let rev'  = case rev of Nothing -> Nothing
                                        Just "" -> Nothing
                                        Just "-" -> Nothing
                                        Just ('-':r) -> Just r
                                        Just _ -> error "The Debian revision needs to start with a dash"
                fmt <- access (D.sourceFormat . A.debInfo)
                -- If no revision number has been set and the format
                -- is not Native3 we need to set it (see
                -- https://github.com/ddssff/cabal-debian/issues/16)
                let rev'' = maybe (case fmt of
                                     Just Native3 -> Nothing
                                     _ -> Just "1") Just rev'
                return $ buildDebianVersion epoch ver rev''

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> CabalT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . getL A.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
finalizeSourceName :: (Monad m, Functor m) => B.PackageType -> CabalT m ()
finalizeSourceName typ =
    do DebBase debName <- debianNameBase
       (D.sourcePackageName . A.debInfo) ~?= Just (SrcPkgName (case typ of
                                                   B.HaskellSource -> "haskell-" ++ debName
                                                   B.Source -> debName
                                                   _ -> error $ "finalizeSourceName: " ++ show typ))

-- | Try to compute a string for the the debian "Maintainer:" and
-- "Uploaders:" fields using, in this order
--    1. the Debian Haskell Group, @pkg-haskell-maintainers\@lists.alioth.debian.org@,
--       if --official is set
--    2. the maintainer explicitly specified using "Debian.Debianize.Monad.maintainer"
--    3. the maintainer field of the cabal package, but only if --official is not set,
--    4. the value returned by getDebianMaintainer, which looks in several environment variables,
--    5. the signature from the latest entry in debian/changelog,
--    6. the Debian Haskell Group, @pkg-haskell-maintainers\@lists.alioth.debian.org@
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Maintainer>
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
finalizeMaintainer :: MonadIO m => CabalT m ()
finalizeMaintainer = do
  o <- access (D.official . A.debInfo)
  currentUser <- liftIO getCurrentDebianUser
  pkgDesc <- access A.packageDescription
  maintainerOption <- access (D.maintainerOption . A.debInfo)
  uploadersOption <- access (D.uploadersOption . A.debInfo)
  let cabalAuthorString = takeWhile (\ c -> c /= ',' && c /= '\n') (Cabal.author pkgDesc)
      cabalMaintainerString = takeWhile (\ c -> c /= ',' && c /= '\n') (Cabal.maintainer pkgDesc)
      cabalMaintainerString' = cabalAuthorString <> " <" <> cabalMaintainerString <> ">"
      cabalMaintainerString'' = cabalAuthorString <> " " <> cabalMaintainerString
  changelogSignature <-
      do log <- access (D.changelog . A.debInfo)
         case log of
           Just (ChangeLog (entry : _)) ->
               case (parseMaintainer (logWho entry)) of
                 Left _e -> return $ Nothing -- Just $ NameAddr (Just "Invalid signature in changelog") (show e)
                 Right x -> return (Just x)
           _ -> return Nothing
  case o of
    True -> do
      (S.maintainer . D.control . A.debInfo) ~= Just haskellMaintainer
      (S.uploaders . D.control . A.debInfo) %= whenEmpty (maybe [] (: []) currentUser)
    False -> do
      (S.maintainer . D.control . A.debInfo) ~?= maintainerOption
      (S.maintainer . D.control . A.debInfo) ~?= (either (const Nothing) Just $ parseMaintainer cabalMaintainerString)
      (S.maintainer . D.control . A.debInfo) ~?= (either (const Nothing) Just $ parseMaintainer cabalMaintainerString')
      (S.maintainer . D.control . A.debInfo) ~?= (either (const Nothing) Just $ parseMaintainer cabalMaintainerString'')
      -- Sometimes the maintainer is just an email, if it matches the author's email we can use it
      (S.maintainer . D.control . A.debInfo) ~?= (case parseMaintainer cabalAuthorString of
                                        Right x | nameAddr_addr x == cabalMaintainerString -> Just x
                                        _ -> Nothing)
      -- Sometimes the maintainer is just an email, try combining it with the author's name
      (S.maintainer . D.control . A.debInfo) ~?= (case parseMaintainer cabalAuthorString of
                                        Right (NameAddr {nameAddr_name = Just name}) -> either (const Nothing) Just (parseMaintainer (name ++ " <" ++ cabalMaintainerString ++ ">"))
                                        _ -> Nothing)
      (S.maintainer . D.control . A.debInfo) ~?= currentUser
      (S.maintainer . D.control . A.debInfo) ~?= changelogSignature
      x <- access (S.maintainer . D.control . A.debInfo)
      when (isNothing x) 
           (do liftIO $ putStrLn ("Unable to construct a debian maintainer, using nobody <nobody@nowhere>. Cabal maintainer strings tried:\n " ++
                                  show cabalMaintainerString ++ ", " ++ show cabalMaintainerString' ++ ", " ++ show cabalMaintainerString'' ++
                                  ", currentUser: " ++ show currentUser)
               return ())
      (S.maintainer . D.control . A.debInfo) ~?= (either (const Nothing) Just $ parseMaintainer "nobody <nobody@nowhere>")
      (S.uploaders . D.control . A.debInfo) %= whenEmpty uploadersOption

-- | If l is the empty list return d, otherwise return l.
whenEmpty :: [a] -> [a] -> [a]
whenEmpty d [] = d
whenEmpty _ l = l

finalizeControl :: (MonadIO m, Functor m) => CabalT m ()
finalizeControl =
    do finalizeMaintainer
       Just src <- access (D.sourcePackageName . A.debInfo)
       (S.source . D.control . A.debInfo) ~= Just src
       desc' <- describe
       (S.xDescription . D.control . A.debInfo) ~?= Just desc'
       -- control %= (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: (MonadIO m, Functor m) => String -> CabalT m ()
finalizeChangelog date =
    do finalizeMaintainer
       ver <- debianVersion
       src <- access (D.sourcePackageName . A.debInfo)
       Just debianMaintainer <- access (S.maintainer . D.control . A.debInfo)
       -- pkgDesc <- access T.packageDescription >>= return . maybe Nothing (either Nothing Just . parseMaintainer . Cabal.maintainer)
       cmts <- access (D.comments . A.debInfo)
       (D.changelog . A.debInfo) %= fmap (dropFutureEntries ver)
       (D.changelog . A.debInfo) %= fixLog src ver cmts debianMaintainer
    where
      -- Ensure that the package name is correct in the first log entry.
      fixLog src ver cmts _maint (Just (ChangeLog (entry : older)))
          | logVersion entry == ver =
              let entry' = entry { logPackage = show (pPrint (PP src))
                                 , logComments = logComments entry ++ "\n" ++
                                                 (List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack) (fromMaybe [] cmts))
                                 } in
              Just (ChangeLog (entry' : older))
      -- The newest log entry isn't exactly ver, build a new entry.
      fixLog src ver cmts maint log =
          let entry = Entry { logPackage = show (pPrint (PP src))
                                 , logVersion = ver
                                 , logDists = [parseReleaseName "UNRELEASED"]
                                 , logUrgency = "low"
                                 , logComments = List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack)
                                                 (fromMaybe [["Debianization generated by cabal-debian"]] cmts)
                                 , logWho = ppDisplay maint
                                 , logDate = date } in
          -- Creating new log entry for version
          Just (ChangeLog (entry : maybe [] (\ (ChangeLog entries) -> entries) log))

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
addExtraLibDependencies hc =
    do pkgDesc <- access A.packageDescription
       devName <- debianName B.Development hc
       libMap <- access (D.extraLibMap . A.debInfo)
       binNames <- List.map (getL B.package) <$> access (S.binaryPackages . D.control . A.debInfo)
       when (any (== devName) binNames) ((B.depends . B.relations . D.binaryDebDescription devName . A.debInfo) %= \ deps -> deps ++ g pkgDesc libMap)
    where
      g :: PackageDescription -> Map String Relations -> Relations
      g pkgDesc libMap = concatMap (devDep libMap) (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)
      devDep :: Map String Relations -> String -> Relations
      devDep libMap cab = maybe [[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]] id (Map.lookup cab libMap)

-- | Applies a few settings to official packages (unless already set)
checkOfficialSettings :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
checkOfficialSettings flavor =
    do o <- access (D.official . A.debInfo)
       when o $ case flavor of
                  GHC -> officialSettings
                  _ -> error $ "There is no official packaging for " ++ show flavor

officialSettings :: (Monad m, Functor m) => CabalT m ()
officialSettings =
    do pkgDesc <- access A.packageDescription
       let PackageName cabal = pkgName (Cabal.package pkgDesc)

       (S.standardsVersion . D.control . A.debInfo) ~?= Just (parseStandardsVersion "3.9.5")
       (S.homepage . D.control . A.debInfo) ~?= Just ("http://hackage.haskell.org/package/" <> pack cabal)
       (D.omitProfVersionDeps . A.debInfo) ~= True
       SrcPkgName src <- access (D.sourcePackageName . A.debInfo) >>= maybe (error "officialSettings: no sourcePackageName") return

       (S.vcsFields . D.control . A.debInfo) %= Set.union (Set.fromList
          [ S.VCSBrowser $ "http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/" <> pack src
          , S.VCSDarcs  $ "http://darcs.debian.org/pkg-haskell/" <> pack src
          ])
 

putBuildDeps :: (MonadIO m, Functor m) => PackageDescription -> CabalT m ()
putBuildDeps pkgDesc =
    do deps <- debianBuildDeps pkgDesc
       depsIndep <- debianBuildDepsIndep pkgDesc
       (S.buildDepends . D.control . A.debInfo) ~= deps
       (S.buildDependsIndep . D.control . A.debInfo) ~= depsIndep

cabalExecBinaryPackage :: Monad m => BinPkgName -> CabalT m ()
cabalExecBinaryPackage b =
    do (B.packageType . D.binaryDebDescription b . A.debInfo) ~?= Just B.Exec
       (B.architecture . D.binaryDebDescription b . A.debInfo) ~?= Just Any
       (B.binarySection . D.binaryDebDescription b . A.debInfo) ~?= Just (MainSection "misc")
       (B.description . D.binaryDebDescription b . A.debInfo) ~?= Just desc -- yeah, this same line is all over the place.
       binaryPackageRelations b B.Exec
    where

binaryPackageRelations :: Monad m => BinPkgName -> B.PackageType -> CabalT m ()
binaryPackageRelations b typ =
    do edds <- access (D.extraDevDeps . A.debInfo)
       (B.depends . B.relations . D.binaryDebDescription b . A.debInfo) %= \ rels ->
          [anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++
          [anyrel "${shlibs:Depends}" | typ `notElem` [B.Profiling, B.Documentation] ] ++
          edds ++ rels
       (B.recommends . B.relations . D.binaryDebDescription b . A.debInfo) %= \ rels -> [anyrel "${haskell:Recommends}"] ++ rels
       (B.suggests . B.relations . D.binaryDebDescription b . A.debInfo) %= \ rels -> [anyrel "${haskell:Suggests}"] ++ rels
       (B.preDepends . B.relations . D.binaryDebDescription b . A.debInfo) ~= []
       (B.breaks . B.relations . D.binaryDebDescription b . A.debInfo) ~= []
       (B.conflicts . B.relations . D.binaryDebDescription b . A.debInfo) %= \ rels -> [anyrel "${haskell:Conflicts}"] ++ rels
       (B.provides . B.relations . D.binaryDebDescription b . A.debInfo) %= \ rels -> (if typ /= B.Documentation then [anyrel "${haskell:Provides}"] else []) ++ rels
       -- T.replaces b %= \ rels -> [anyrel "${haskell:Replaces}"] ++ rels
       (B.builtUsing . B.relations . D.binaryDebDescription b . A.debInfo) ~= []

-- | Add the library paragraphs for a particular compiler flavor.
librarySpecs :: (Monad m, Functor m) => PackageDescription -> CompilerFlavor -> CabalT m ()
librarySpecs pkgDesc hc =
    do let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . getL (D.noDocumentationLibrary . A.debInfo)
       prof <- get >>= return . not . getL (D.noProfilingLibrary . A.debInfo)
       when dev (librarySpec Any B.Development hc)
       when (dev && prof && hc == GHC) (librarySpec Any B.Profiling hc)
       when (dev && doc) (docSpecsParagraph hc)

docSpecsParagraph :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
docSpecsParagraph hc =
    do b <- debianName B.Documentation hc
       binaryPackageRelations b B.Documentation
       (B.packageType . D.binaryDebDescription b . A.debInfo) ~?= Just B.Documentation
       (B.packageType . D.binaryDebDescription b . A.debInfo) ~?= Just B.Documentation
       (B.architecture . D.binaryDebDescription b . A.debInfo) ~= Just All
       (B.binarySection . D.binaryDebDescription b . A.debInfo) ~?= Just (MainSection "doc")
       (B.description . D.binaryDebDescription b . A.debInfo) ~?= Just desc

librarySpec :: (Monad m, Functor m) => PackageArchitectures -> B.PackageType -> CompilerFlavor -> CabalT m ()
librarySpec arch typ hc =
    do b <- debianName typ hc
       binaryPackageRelations b typ
       (B.packageType . D.binaryDebDescription b . A.debInfo) ~?= Just typ
       (B.packageType . D.binaryDebDescription b . A.debInfo) ~?= Just typ
       (B.architecture . D.binaryDebDescription b . A.debInfo) ~?= Just arch
       (B.description . D.binaryDebDescription b . A.debInfo) ~?= Just desc

-- | This is the standard value for the Description field of a binary
-- package control file stanza.
desc :: Text
desc = Text.intercalate "\n "
         ["${haskell:ShortDescription}${haskell:ShortBlurb}",
          "${haskell:LongDescription}",
          ".",
          "${haskell:Blurb}"]

-- | Make sure all data and executable files are assigned to at least
-- one binary package and make sure all binary packages are in the
-- package list in the source deb description.  If there are left over
-- files, assign them to the packages returned by the
-- utilsPackageNames lens, and make sure those packages are in the
-- source deb description.
makeUtilsPackage :: forall m. (MonadIO m, Functor m) => PackageDescription -> CompilerFlavor -> CabalT m ()
makeUtilsPackage pkgDesc hc =
    do -- Files the cabal package expects to be installed
       -- Files that are already assigned to any binary deb
       installedDataMap <- Set.fold (\ x r ->
                                         case x of
                                           D.Install b from _ -> Map.insertWith Set.union b (singleton from) r
                                           D.InstallTo b from _ -> Map.insertWith Set.union b (singleton from) r
                                           D.InstallData b from _ -> Map.insertWith Set.union b (singleton from) r
                                           _ -> r) mempty <$> access (D.atomSet . A.debInfo) :: CabalT m (Map BinPkgName (Set FilePath))
       installedExecMap <- Set.fold (\ x r ->
                                         case x of
                                           D.InstallCabalExec b name _ -> Map.insertWith Set.union b (singleton name) r
                                           D.InstallCabalExecTo b name _ -> Map.insertWith Set.union b (singleton name) r
                                           _ -> r) mempty <$> access (D.atomSet . A.debInfo) :: CabalT m (Map BinPkgName (Set String))

       -- The names of cabal executables that go into eponymous debs
       insExecPkg <- access (D.executable . A.debInfo) >>= return . Set.map ename . Set.fromList . elems

       let installedData = Set.map (\ a -> (a, a)) $ Set.unions (Map.elems installedDataMap)
           installedExec = Set.unions (Map.elems installedExecMap)

       let prefixPath = Cabal.dataDir pkgDesc
       let dataFilePaths = Set.fromList (zip (List.map (prefixPath </>) (Cabal.dataFiles pkgDesc)) (Cabal.dataFiles pkgDesc)) :: Set (FilePath, FilePath)
           execFilePaths = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc))) :: Set FilePath
       let availableData = Set.union installedData dataFilePaths
           availableExec = Set.union installedExec execFilePaths

       access (D.utilsPackageNameBase . A.debInfo) >>= \ name ->
           case name of
             Nothing -> debianName B.Utilities hc >>= \ (BinPkgName name') -> (D.utilsPackageNameBase . A.debInfo) ~= Just name'
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
         (B.description . D.binaryDebDescription b . A.debInfo) ~?= Just desc
         -- This is really for all binary debs except the libraries - I'm not sure why
         (D.rulesFragments . A.debInfo)+= (pack ("build" </> ppDisplay b ++ ":: build-ghc-stamp\n"))
         (B.architecture . D.binaryDebDescription b . A.debInfo) ~?= Just (if Set.null utilsExec then All else Any)
         (B.binarySection . D.binaryDebDescription b . A.debInfo) ~?= Just (MainSection "misc")
         binaryPackageRelations b B.Utilities
       -- Add the unassigned files to the utils packages
       Set.mapM_ (\ (foo, bar) -> (D.atomSet . A.debInfo) %= (Set.insert $ D.InstallData b foo bar)) utilsDataMissing
       Set.mapM_ (\ name -> (D.atomSet . A.debInfo) %= (Set.insert $ D.InstallCabalExec b name "usr/bin")) utilsExecMissing
    where
      ename i =
          case D.sourceDir i of
            (Nothing) -> D.execName i
            (Just s) ->  s </> D.execName i

expandAtoms :: MonadIO m => CabalT m ()
expandAtoms =
    do hc <- access (compilerFlavor . D.flags . A.debInfo)
       case hc of
         GHC -> (cabalFlagAssignments . D.flags . A.debInfo) %= (Set.union (Set.fromList (flagList "--ghc")))
         GHCJS -> (cabalFlagAssignments . D.flags . A.debInfo) %= (Set.union (Set.fromList (flagList "--ghcjs")))
       builddir <- access (D.buildDir . A.debInfo) >>= return . fromMaybe (case hc of
                                                               GHC -> "dist-ghc/build"
                                                               GHCJS -> "dist-ghcjs/build"
                                                               _ -> error $ "Unexpected compiler: " ++ show hc)
       dDir <- dataDir
       expandApacheSites
       expandInstallCabalExecs builddir
       expandInstallCabalExecTo builddir
       expandInstallData dDir
       expandInstallTo
       expandFile
       expandWebsite
       expandServer
       expandBackups
       expandExecutable
    where
      expandApacheSites :: Monad m => CabalT m ()
      expandApacheSites =
          do mp <- get >>= return . getL (D.apacheSite . A.debInfo)
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do (D.atomSet . A.debInfo) %= (Set.insert $ D.Link b ("/etc/apache2/sites-available/" ++ dom) ("/etc/apache2/sites-enabled/" ++ dom))
                   (D.atomSet . A.debInfo) %= (Set.insert $ D.InstallDir b log)
                   (D.atomSet . A.debInfo) %= (Set.insert $ D.File b ("/etc/apache2/sites-available" </> dom) text)

      -- Turn A.InstallCabalExec into A.Install
      expandInstallCabalExecs :: Monad m => FilePath -> CabalT m ()
      expandInstallCabalExecs builddir = do
        hc <- access (compilerFlavor . D.flags . A.debInfo)
        access (D.atomSet . A.debInfo) >>= Set.mapM_ (doAtom hc)
          where
            doAtom :: Monad m => CompilerFlavor -> D.Atom -> CabalT m ()
            doAtom GHC (D.InstallCabalExec b name dest) = (D.atomSet . A.debInfo) %= (Set.insert $ D.Install b (builddir </> name </> name) dest)
            -- A GHCJS executable is a directory with files, copy them
            -- all into place.
            doAtom GHCJS (D.InstallCabalExec b name dest) =
                (D.rulesFragments . A.debInfo) +=
                     (Text.unlines
                        [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                        , pack ("\t(cd " <> builddir </> name <> " && find " <> name <.> "jsexe" <> " -type f) |\\\n" <>
                                       "\t  while read i; do install -Dp " <> builddir </> name </> "$$i debian" </> ppDisplay b </> makeRelative "/" dest </> "$$i; done\n") ])
            doAtom _ _ = return ()

      -- Turn A.InstallCabalExecTo into a make rule
      expandInstallCabalExecTo :: Monad m => FilePath -> CabalT m ()
      expandInstallCabalExecTo builddir = do
        hc <- access (compilerFlavor . D.flags . A.debInfo)
        access (D.atomSet . A.debInfo) >>= Set.mapM_ (doAtom hc)
          where
            doAtom :: Monad m => CompilerFlavor -> D.Atom -> CabalT m ()
            doAtom GHC (D.InstallCabalExecTo b name dest) =
                (D.rulesFragments . A.debInfo) +=
                                     (Text.unlines
                                       [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                                       , "\tinstall -Dps " <> pack (builddir </> name </> name) <> " "
                                                           <> pack ("debian" </> ppDisplay b </> makeRelative "/" dest) ])
            doAtom hc (D.InstallCabalExecTo b name dest) = error $ "expandInstallCabalExecTo " ++ show hc ++ " " ++ show (D.InstallCabalExecTo b name dest)
            doAtom _ _ = return ()

      -- Turn A.InstallData into either an Install or an InstallTo
      expandInstallData :: Monad m => FilePath -> CabalT m ()
      expandInstallData dDir =
          access (D.atomSet . A.debInfo) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.InstallData b from dest) =
                if takeFileName from == takeFileName dest
                then (D.atomSet . A.debInfo) %= (Set.insert $ D.Install b from (dDir </> makeRelative "/" (takeDirectory dest)))
                else (D.atomSet . A.debInfo) %= (Set.insert $ D.InstallTo b from (dDir </> makeRelative "/" dest))
            doAtom _ = return ()

      -- Turn A.InstallTo into a make rule
      expandInstallTo :: Monad m => CabalT m ()
      expandInstallTo =
          access (D.atomSet . A.debInfo) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.InstallTo b from dest) =
                (D.rulesFragments . A.debInfo) +=
                                    (Text.unlines [ pack ("binary-fixup" </> ppDisplay b) <> "::"
                                                  , "\tinstall -Dp " <> pack from <> " " <> pack ("debian" </> ppDisplay b </> makeRelative "/" dest) ])
            doAtom _ = return ()

      -- Turn A.File into an intermediateFile and an A.Install
      expandFile :: Monad m => CabalT m ()
      expandFile =
          access (D.atomSet . A.debInfo) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.File b path text) =
                do let (destDir', destName') = splitFileName path
                       tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack text)))
                       tmpPath = tmpDir </> destName'
                   (D.intermediateFiles . A.debInfo) += (tmpPath, text)
                   (D.atomSet . A.debInfo) %= (Set.insert $ D.Install b tmpPath destDir')
            doAtom _ = return ()

      expandWebsite :: Monad m => CabalT m ()
      expandWebsite =
          do mp <- get >>= return . getL (D.website . A.debInfo)
             List.mapM_ (\ (b, site) -> modify (siteAtoms b site)) (Map.toList mp)

      expandServer :: Monad m => CabalT m ()
      expandServer =
          do mp <- get >>= return . getL (D.serverInfo . A.debInfo)
             List.mapM_ (\ (b, x) -> modify (serverAtoms b x False)) (Map.toList mp)

      expandBackups :: Monad m => CabalT m ()
      expandBackups =
          do mp <- get >>= return . getL (D.backups . A.debInfo)
             List.mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)

      expandExecutable :: Monad m => CabalT m ()
      expandExecutable =
          do mp <- get >>= return . getL (D.executable . A.debInfo)
             List.mapM_ (\ (b, f) -> modify (execAtoms b f)) (Map.toList mp)

-- | Add the normal default values to the rules files.
finalizeRules :: (Monad m, Functor m) => CabalT m ()
finalizeRules =
    do DebBase b <- debianNameBase
       compiler <- access (compilerFlavor . D.flags . A.debInfo)
       (D.rulesHead . A.debInfo) ~?= Just "#!/usr/bin/make -f"
       (D.rulesSettings . A.debInfo) %= (++ ["DEB_CABAL_PACKAGE = " <> pack b])
       (D.rulesSettings . A.debInfo) %= (++ (["DEB_DEFAULT_COMPILER = " <> pack (List.map toLower (show compiler))]))
       flags <- (flagString . Set.toList) <$> access (cabalFlagAssignments . D.flags . A.debInfo)
       unless (List.null flags) ((D.rulesSettings . A.debInfo) %= (++ ["DEB_SETUP_GHC6_CONFIGURE_ARGS = " <> pack flags]))
       (D.rulesIncludes . A.debInfo) %= (++ ["include /usr/share/cdbs/1/rules/debhelper.mk",
                                             "include /usr/share/cdbs/1/class/hlibrary.mk"])

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

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = List.map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (List.map toLower name), False)
        tagWithValue name       = (FlagName (List.map toLower name), True)

flagString :: [(FlagName, Bool)] -> String
flagString = List.intercalate " " . List.map (\ (FlagName s, sense) -> "-f" ++ (if sense then "" else "-") ++ s)
