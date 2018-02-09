-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.Finalize
    ( debianize
    -- , finalizeDebianization -- external use deprecated - used in test script
    ) where


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
#endif
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)
import Control.Monad as List (mapM_)
import Control.Monad.State (get, modify)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List as List (filter, intercalate, map, nub, null, unlines, maximumBy)
import Data.Map as Map (delete, elems, insertWith, lookup, Map, toList)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Monoid ((<>))
import Data.Set as Set (difference, filter, fold, fromList, insert, map, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (mapM_)
import Data.Text as Text (intercalate, pack, Text, unlines, unpack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.BasicInfo (cabalFlagAssignments, compilerFlavor, verbosity)
import qualified Debian.Debianize.BinaryDebDescription as B
import Debian.Debianize.BuildDependencies (debianBuildDeps, debianBuildDepsIndep)
import qualified Debian.Debianize.CabalInfo as A
import Debian.Debianize.Changelog (dropFutureEntries)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.DebianName (debianName, debianNameBase)
import Debian.Debianize.Goodies (backupAtoms, describe, execAtoms, serverAtoms, siteAtoms, watchAtom)
import Debian.Debianize.InputDebian (dataTop, dataDest, inputChangeLog)
import Debian.Debianize.Monad as Monad (CabalT, liftCabal)
import Debian.Debianize.Prelude ((.?=))
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.GHC (compilerPackageName)
import Debian.Orphans ()
import Debian.Policy (getCurrentDebianUser, getDebhelperCompatLevel, haskellMaintainer, maintainerOfLastResort, PackageArchitectures(Any, All), PackagePriority(Optional), parseMaintainer, parseStandardsVersion, Section(..), SourceFormat(Native3))
import Debian.Pretty (PP(..), ppShow)
import Debian.Relation (BinPkgName, BinPkgName(BinPkgName), Relation(Rel), Relations, SrcPkgName(SrcPkgName))
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..))
import Debian.Release (parseReleaseName)
import Debian.Time (getCurrentLocalRFC822Time)
import qualified Debian.Version as V (buildDebianVersion, DebianVersion, parseDebianVersion', epoch, version, revision)
import Distribution.Compiler (CompilerFlavor(GHC))
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (CompilerFlavor(GHCJS))
#endif
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName, mkPackageName, unPackageName)
import Distribution.PackageDescription as Cabal (allBuildInfo, author, BuildInfo(buildable, extraLibs), Executable(buildInfo, exeName), FlagName, mkFlagName, unFlagName, maintainer, PackageDescription(testSuites))
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText
#else
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (allBuildInfo, author, BuildInfo(buildable, extraLibs), Executable(buildInfo, exeName), FlagName(FlagName), maintainer, PackageDescription(testSuites))
#endif
import qualified Distribution.PackageDescription as Cabal (PackageDescription(dataFiles, executables, library, package))
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>), makeRelative, splitFileName, takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
#if MIN_VERSION_hsemail(2,0,0)
import Text.Parsec.Rfc2822 (NameAddr(..))
#else
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
#endif
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

-- | @debianize customize@ initializes the CabalT state from the
-- environment and the cabal package description in (and possibly the
-- debian/changelog file) from the current directory, then runs
-- @customize@ and finalizes the debianization so it is ready to be
-- output.
debianize :: (MonadIO m, Functor m) => CabalT m () -> CabalT m ()
debianize customize =
  do liftCabal inputChangeLog
     customize
     finalizeDebianization

-- | Do some light IO and call finalizeDebianization.
finalizeDebianization :: (MonadIO m, Functor m) => CabalT m ()
finalizeDebianization =
    do date <- liftIO getCurrentLocalRFC822Time
       currentUser <- liftIO getCurrentDebianUser
       debhelperCompat <- liftIO getDebhelperCompatLevel
       setupExists <- or <$> mapM (liftIO . doesFileExist) ["Setup.hs", "Setup.lhs"]
       finalizeDebianization' date currentUser debhelperCompat setupExists
       vb <- use (A.debInfo . D.flags . verbosity)
       when (vb >= 3) (get >>= \ x -> liftIO (putStrLn ("\nFinalized Cabal Info: " ++ show x ++ "\n")))
       either (\e -> liftIO $ hPutStrLn stderr ("WARNING: " ++ e)) (\_ -> return ()) =<< use (A.debInfo . D.control . S.maintainer)

-- | Now that we know the build and data directories, we can expand
-- some atoms into sets of simpler atoms which can eventually be
-- turned into the files of the debianization.  The original atoms are
-- not removed from the list because they may contribute to the
-- debianization in other ways, so be careful not to do this twice,
-- this function is not idempotent.  (Exported for use in unit tests.)
-- FIXME: we should be able to run this without a PackageDescription, change
--        paramter type to Maybe PackageDescription and propagate down thru code
finalizeDebianization'  :: (MonadIO m, Functor m) => String -> Maybe NameAddr -> Maybe Int -> Bool -> CabalT m ()
finalizeDebianization' date currentUser debhelperCompat setupExists =
    do -- In reality, hcs must be a singleton or many things won't work.  But some day...
       hc <- use (A.debInfo . D.flags . compilerFlavor)
       pkgDesc <- use A.packageDescription

       testsStatus <- use (A.debInfo . D.testsStatus)
       let testsExist = not $ List.null $ Cabal.testSuites pkgDesc
       case (testsExist, testsStatus) of
         (True, D.TestsRun) -> (A.debInfo . D.rulesSettings) %= (++ ["DEB_ENABLE_TESTS = yes"])
         (True, D.TestsBuild) -> (A.debInfo . D.rulesSettings) %= (++ ["DEB_ENABLE_TESTS = yes", "DEB_BUILD_OPTIONS += nocheck"])
         _ -> return ()
       (A.debInfo . D.rulesSettings) %=
          (++ ["DEB_SETUP_BIN_NAME = " <> if setupExists then "debian/hlibrary.setup" else "cabal"])
 
       finalizeSourceName B.HaskellSource
       checkOfficialSettings hc
       addExtraLibDependencies hc
       (A.debInfo . D.watch) .?= Just (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       (A.debInfo . D.control . S.section) .?= Just (MainSection "haskell")
       (A.debInfo . D.control . S.priority) .?= Just Optional
       (A.debInfo . D.compat) .?= debhelperCompat
       finalizeChangelog date currentUser
       finalizeControl currentUser
       finalizeRules
       -- T.license .?= Just (Cabal.license pkgDesc)
       expandAtoms
       -- Create the binary packages for the web sites, servers, backup packges, and other executables
       use (A.debInfo . D.executable) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       use (A.debInfo . D.backups) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       use (A.debInfo . D.serverInfo) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       use (A.debInfo . D.website) >>= List.mapM_ (cabalExecBinaryPackage . fst) . Map.toList
       -- Make sure all the control file sections exist before doing the build dependencies,
       -- because we need to filter out self dependencies.
       librarySpecs pkgDesc hc
       makeUtilsPackage pkgDesc hc
       debs <- use (A.debInfo . D.control . S.binaryPackages) >>= return . List.map (view B.package)
       allowSelfDeps <- use (A.debInfo . D.allowDebianSelfBuildDeps)
       putBuildDeps (if allowSelfDeps then id else filterRelations debs) pkgDesc
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
finalizeDescriptions = use T.binaryPackages >>= List.mapM_ finalizeDescription

finalizeDescription :: (Monad m, Functor m) => B.BinaryDebDescription -> CabalT m ()
finalizeDescription bdd =
    do let b = view B.package bdd
       cabDesc <- describe
       T.debianDescription .?= Just cabDesc
-}

-- | Construct the final Debian version number.
--  Inputs:
--
--    1. --deb-version argument
--    2. --revision argument
--    3. cabal version number
--    4. latest version in debian/changelog
--
-- The --deb-version argument overrides everything.
debianVersion :: (Monad m, Functor m) => CabalT m V.DebianVersion
debianVersion =
    do cabalName <- (pkgName . Cabal.package) <$> use A.packageDescription
       (cabalVersion :: V.DebianVersion) <- (V.parseDebianVersion' . ppShow . pkgVersion . Cabal.package) <$> use A.packageDescription
       cabalEpoch <- debianEpoch cabalName
       fmt <- use (A.debInfo . D.sourceFormat)
       cabalRevision <-
           do x <- use (A.debInfo . D.revision) -- from the --revision option
              let y = case x of
                        Nothing -> Nothing
                        Just "" -> Nothing
                        Just "-" -> Nothing
                        Just ('-':r) -> Just r
                        Just _ -> error "The --revision argument must start with a dash"
              return $ case fmt of
                         Native3 -> y
                         _ -> maybe (Just "1") (Just . max "1") y
       versionArg <- use (A.debInfo . D.debVersion) -- from the --deb-version option
       (debVersion :: Maybe V.DebianVersion) <- use (A.debInfo . D.changelog) >>= return . maybe Nothing changelogVersion

       case () of
         _ | maybe False (\ v -> v < V.buildDebianVersion cabalEpoch (ppShow cabalVersion) Nothing) versionArg ->
               error ("Version from --deb-version (" ++ ppShow versionArg ++
                      ") is older than cabal version (" ++ ppShow cabalVersion ++
                      "), maybe you need to unpin this package?")
         _ | isJust versionArg -> return $ fromJust versionArg
         _ | isJust debVersion ->
               case (V.epoch (fromJust debVersion),
                     V.parseDebianVersion' (V.version (fromJust debVersion)),
                     V.revision (fromJust debVersion)) of
                 (debEpoch, debianVersion', (debianRevision :: Maybe String)) ->
                     let finalEpoch = max debEpoch cabalEpoch
                         finalVersion = max debianVersion' cabalVersion
                         (finalRevision :: Maybe String) = maximumBy (compare `on` fmap V.parseDebianVersion') [debianRevision, cabalRevision] in
                     return $ V.buildDebianVersion finalEpoch (ppShow finalVersion) finalRevision
         _ -> return $ V.buildDebianVersion cabalEpoch (ppShow cabalVersion) cabalRevision

changelogVersion :: ChangeLog -> Maybe V.DebianVersion
changelogVersion (ChangeLog (Entry {logVersion = x} : _)) = Just x
changelogVersion _ = Nothing

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> CabalT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . view A.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
finalizeSourceName :: (Monad m, Functor m) => B.PackageType -> CabalT m ()
finalizeSourceName typ =
    do DebBase debName <- debianNameBase
       hc <- use (A.debInfo . D.flags . compilerFlavor)
       (A.debInfo . D.sourcePackageName) .?=
          Just (SrcPkgName (case (hc, typ) of
                              -- Haskell source deb names conventionally have the prefix
                              -- "haskell-" added.  Here we add prefix "ghcjs-" to
                              -- haskell packages build with the ghcjs compiler.
                              (GHC, B.HaskellSource) -> "haskell-" ++ debName
#if MIN_VERSION_Cabal(1,22,0)
                              (GHCJS, B.HaskellSource) -> "ghcjs-" ++ debName
#endif
                              (_, B.Source) -> debName
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
finalizeMaintainer :: Monad m => Maybe NameAddr -> CabalT m ()
finalizeMaintainer currentUser = do
  o <- use (A.debInfo . D.official)
  pkgDesc <- use A.packageDescription
  maintainerOption <- use (A.debInfo . D.maintainerOption)
  uploadersOption <- use (A.debInfo . D.uploadersOption)
  let cabalAuthorString = takeWhile (\ c -> c /= ',' && c /= '\n') (Cabal.author pkgDesc)
      cabalMaintainerString = takeWhile (\ c -> c /= ',' && c /= '\n') (Cabal.maintainer pkgDesc)
      cabalMaintainerString' = cabalAuthorString <> " <" <> cabalMaintainerString <> ">"
      cabalMaintainerString'' = cabalAuthorString <> " " <> cabalMaintainerString
  changelogSignature <-
      do log <- use (A.debInfo . D.changelog)
         case log of
           Just (ChangeLog (entry : _)) ->
               case (parseMaintainer (logWho entry)) of
                 Left _e -> return $ Nothing -- Just $ NameAddr (Just "Invalid signature in changelog") (show e)
                 Right x -> return (Just x)
           _ -> return Nothing
  case o of
    True -> do
      (A.debInfo . D.control . S.maintainer) .= Right haskellMaintainer
      (A.debInfo . D.control . S.uploaders) %= whenEmpty (maybe [] (: []) currentUser)
    False -> do
      (A.debInfo . D.control . S.maintainer) %= either (\x -> maybe (Left x) Right maintainerOption) Right
      (A.debInfo . D.control . S.maintainer) %= either (\_ -> parseMaintainer cabalMaintainerString) Right
      (A.debInfo . D.control . S.maintainer) %= either (\_ -> parseMaintainer cabalMaintainerString') Right
      (A.debInfo . D.control . S.maintainer) %= either (\_ -> parseMaintainer cabalMaintainerString'') Right
      -- Sometimes the maintainer is just an email, if it matches the author's email we can use it
      (A.debInfo . D.control . S.maintainer) %= either (\e -> case parseMaintainer cabalAuthorString of
                                                                Right x | nameAddr_addr x == cabalMaintainerString -> Right x
                                                                Right _ -> Left e
                                                                Left x -> Left x) Right
      -- Sometimes the maintainer is just an email, try combining it with the author's name
      (A.debInfo . D.control . S.maintainer) %= either (\e -> case parseMaintainer cabalAuthorString of
                                                                Right (NameAddr {nameAddr_name = Just name}) -> parseMaintainer (name ++ " <" ++ cabalMaintainerString ++ ">")
                                                                Right _ -> Left e
                                                                Left x -> Left x) Right
      (A.debInfo . D.control . S.maintainer) %= either (\e -> maybe (Left e) Right currentUser) Right
      (A.debInfo . D.control . S.maintainer) %= either (\e -> maybe (Left e) Right changelogSignature) Right
      (A.debInfo . D.control . S.maintainer) %= either (\_ -> Left ("Unable to construct a debian maintainer, using default.  Cabal maintainer strings tried:\n " ++
                                                                    show cabalMaintainerString ++ ", " ++ show cabalMaintainerString' ++ ", " ++ show cabalMaintainerString'' ++
                                                                    ", currentUser: " ++ show currentUser)) Right
      (A.debInfo . D.control . S.uploaders) %= whenEmpty uploadersOption

-- | If l is the empty list return d, otherwise return l.
whenEmpty :: [a] -> [a] -> [a]
whenEmpty d [] = d
whenEmpty _ l = l

finalizeControl :: (Monad m, Functor m) => Maybe NameAddr -> CabalT m ()
finalizeControl currentUser =
    do finalizeMaintainer currentUser
       Just src <- use (A.debInfo . D.sourcePackageName)
       (A.debInfo . D.control . S.source) .= Just src
       desc' <- describe
       (A.debInfo . D.control . S.xDescription) .?= Just desc'
       -- control %= (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: (Monad m, Functor m) => String -> Maybe NameAddr -> CabalT m ()
finalizeChangelog date currentUser =
    do finalizeMaintainer currentUser
       ver <- debianVersion
       src <- use (A.debInfo . D.sourcePackageName)
       debianUploaders  <- use (A.debInfo . D.control . S.uploaders)
       debianMaintainer <- use (A.debInfo . D.control . S.maintainer)
       let nameToUse | (n:_) <- debianUploaders = Right n
                     | otherwise                = debianMaintainer
       -- pkgDesc <- use T.packageDescription >>= return . maybe Nothing (either Nothing Just . parseMaintainer . Cabal.maintainer)
       cmts <- use (A.debInfo . D.comments)
       (A.debInfo . D.changelog) %= fmap (dropFutureEntries ver)
       let msg = "Initial release"
       (A.debInfo . D.changelog) %= fixLog src ver cmts nameToUse msg
    where
      fixLog :: Maybe SrcPkgName -> V.DebianVersion -> Maybe [[Text]] -> Either String NameAddr -> Text -> Maybe ChangeLog -> Maybe ChangeLog
      -- Ensure that the package name is correct in the first log entry.
      fixLog src ver cmts _maint _ (Just (ChangeLog (entry : older)))
          | logVersion entry == ver =
              let entry' = entry { logPackage = show (pPrint (PP src))
                                 , logComments = logComments entry ++ "\n" ++
                                                 (List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack) (fromMaybe [] cmts))
                                 } in
              Just (ChangeLog (entry' : older))
      -- The newest log entry isn't exactly ver, build a new entry.
      fixLog src ver cmts maint msg log =
          let entry = Entry { logPackage = show (pPrint (PP src))
                                 , logVersion = ver
                                 , logDists = [parseReleaseName "UNRELEASED"]
                                 , logUrgency = "low"
                                 , logComments =
                                     List.unlines $ List.map (("  * " <>) . List.intercalate "\n    " . List.map unpack) (fromMaybe [[msg]] cmts)
                                 , logWho = either (\_ -> ppShow maintainerOfLastResort) ppShow maint
                                 , logDate = date } in
          -- Creating new log entry for version
          Just (ChangeLog (entry : maybe [] (\ (ChangeLog entries) -> entries) log))

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
addExtraLibDependencies hc =
    do pkgDesc <- use A.packageDescription
       devName <- debianName B.Development hc
       libMap <- use (A.debInfo . D.extraLibMap)
       binNames <- List.map (view B.package) <$> use (A.debInfo . D.control . S.binaryPackages)
       when (any (== devName) binNames) ((A.debInfo . D.binaryDebDescription devName . B.relations . B.depends) %= \ deps -> deps ++ g pkgDesc libMap)
    where
      g :: PackageDescription -> Map String Relations -> Relations
      g pkgDesc libMap = concatMap (devDep libMap) (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)
      devDep :: Map String Relations -> String -> Relations
      devDep libMap cab = maybe [[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]] id (Map.lookup cab libMap)

-- | Applies a few settings to official packages (unless already set)
checkOfficialSettings :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
checkOfficialSettings flavor =
    do o <- use (A.debInfo . D.official)
       when o $ case flavor of
                  GHC -> officialSettings
                  _ -> error $ "There is no official packaging for " ++ show flavor

officialSettings :: (Monad m, Functor m) => CabalT m ()
officialSettings = do
    pkgDesc <- use A.packageDescription
#if MIN_VERSION_Cabal(2,0,0)
    let cabal = pkgName (Cabal.package pkgDesc)
#else
    let PackageName cabal = pkgName (Cabal.package pkgDesc)
#endif
    zoom A.debInfo $ do
        let officialError = error "officialSettings: no sourcePackageName"

        D.omitProfVersionDeps .= True
        SrcPkgName src <- fromMaybe officialError <$> use D.sourcePackageName

        let packagesURI = "https://salsa.debian.org/haskell-team/DHG_packages/tree/master/p/" <> pack src
        zoom D.control $ do
           S.standardsVersion .?= Just (parseStandardsVersion "4.1.1")
#if MIN_VERSION_Cabal(2,0,0)
           S.homepage .?= Just ("https://hackage.haskell.org/package/" <> pack (unPackageName cabal))
#else
           S.homepage .?= Just ("https://hackage.haskell.org/package/" <> pack cabal)
#endif
           S.vcsFields %= Set.union (Set.fromList
              [ S.VCSBrowser packagesURI
              , S.VCSGit  "https://salsa.debian.org/haskell-team/DHG_packages.git"
              ])

putBuildDeps :: (MonadIO m, Functor m) => (Relations -> Relations) -> PackageDescription -> CabalT m ()
putBuildDeps finalizeRelations pkgDesc =
    do deps <- debianBuildDeps pkgDesc >>= return . finalizeRelations
       depsIndep <- debianBuildDepsIndep pkgDesc >>= return . finalizeRelations
       (A.debInfo . D.control . S.buildDepends) .= deps
       (A.debInfo . D.control . S.buildDependsIndep) .= depsIndep

-- | Filter out any relations that mention any of the bad package names.
filterRelations :: [BinPkgName] -> Relations -> Relations
filterRelations badNames orRels =
    List.filter (not . List.null) (List.map filterOrRelation orRels)
    where
      filterOrRelation :: [Relation] -> [Relation]
      filterOrRelation rels = List.filter (\ (Rel name _ _) -> not (elem name badNames)) rels

cabalExecBinaryPackage :: Monad m => BinPkgName -> CabalT m ()
cabalExecBinaryPackage b =
    do (A.debInfo . D.binaryDebDescription b . B.packageType) .?= Just B.Exec
       (A.debInfo . D.binaryDebDescription b . B.architecture) .?= Just Any
       (A.debInfo . D.binaryDebDescription b . B.binarySection) .?= Just (MainSection "misc")
       (A.debInfo . D.binaryDebDescription b . B.description) .?= Just desc -- yeah, this same line is all over the place.
       binaryPackageRelations b B.Exec
    where

binaryPackageRelations :: Monad m => BinPkgName -> B.PackageType -> CabalT m ()
binaryPackageRelations b typ = zoom A.debInfo $ do
  edds <- use D.extraDevDeps
  zoom (D.binaryDebDescription b . B.relations) $ do
    when (typ == B.Development) $ do
      B.depends %= (edds ++)
      B.depends %= (anyrel "${shlibs:Depends}" : )
    when (typ == B.Utilities) $
      B.depends %= (anyrel "${shlibs:Depends}" : )
    B.depends    %= ([anyrel "${haskell:Depends}", anyrel "${misc:Depends}"] ++)
    B.recommends %= (anyrel "${haskell:Recommends}" : )
    B.suggests   %= (anyrel "${haskell:Suggests}" :)
    B.conflicts  %= (anyrel "${haskell:Conflicts}" :)
    B.preDepends .= []
    B.breaks     .= []
    B.builtUsing .= []

    unless (typ == B.Documentation) $ do
      B.provides %= (anyrel "${haskell:Provides}" :)

-- | Add the library paragraphs for a particular compiler flavor.
librarySpecs :: (Monad m, Functor m) => PackageDescription -> CompilerFlavor -> CabalT m ()
librarySpecs pkgDesc hc =
    do let dev = isJust (Cabal.library pkgDesc)
       doc <- get >>= return . not . view (A.debInfo . D.noDocumentationLibrary)
       prof <- get >>= return . not . view (A.debInfo . D.noProfilingLibrary)
       when dev (librarySpec Any B.Development hc)
       when (dev && prof && hc == GHC) (librarySpec Any B.Profiling hc)
       when (dev && doc) (docSpecsParagraph hc)

docSpecsParagraph :: (Monad m, Functor m) => CompilerFlavor -> CabalT m ()
docSpecsParagraph hc =
    do b <- debianName B.Documentation hc
       binaryPackageRelations b B.Documentation
       (A.debInfo . D.binaryDebDescription b . B.packageType) .?= Just B.Documentation
       (A.debInfo . D.binaryDebDescription b . B.packageType) .?= Just B.Documentation
       (A.debInfo . D.binaryDebDescription b . B.architecture) .= Just All
       (A.debInfo . D.binaryDebDescription b . B.binarySection) .?= Just (MainSection "doc")
       (A.debInfo . D.binaryDebDescription b . B.description) .?= Just desc

librarySpec :: (Monad m, Functor m) => PackageArchitectures -> B.PackageType -> CompilerFlavor -> CabalT m ()
librarySpec arch typ hc =
    do b <- debianName typ hc
       binaryPackageRelations b typ
       (A.debInfo . D.binaryDebDescription b . B.packageType) .?= Just typ
       (A.debInfo . D.binaryDebDescription b . B.packageType) .?= Just typ
       (A.debInfo . D.binaryDebDescription b . B.architecture) .?= Just arch
       (A.debInfo . D.binaryDebDescription b . B.description) .?= Just desc

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
makeUtilsPackage :: forall m. (Monad m, Functor m) => PackageDescription -> CompilerFlavor -> CabalT m ()
makeUtilsPackage pkgDesc hc =
    do -- Files the cabal package expects to be installed
       -- Files that are already assigned to any binary deb
       installedDataMap <- Set.fold (\ x r ->
                                         case x of
                                           D.Install b src _ -> Map.insertWith Set.union b (singleton src) r
                                           D.InstallTo b src _ -> Map.insertWith Set.union b (singleton src) r
                                           D.InstallData b src  _ -> Map.insertWith Set.union b (singleton src) r
                                           _ -> r) mempty <$> use (A.debInfo . D.atomSet) :: CabalT m (Map BinPkgName (Set FilePath))
       installedExecMap <- Set.fold (\ x r ->
                                         case x of
                                           D.InstallCabalExec b name _ -> Map.insertWith Set.union b (singleton name) r
                                           D.InstallCabalExecTo b name _ -> Map.insertWith Set.union b (singleton name) r
                                           _ -> r) mempty <$> use (A.debInfo . D.atomSet) :: CabalT m (Map BinPkgName (Set String))

       -- The names of cabal executables that go into eponymous debs
       insExecPkg <- use (A.debInfo . D.executable) >>= return . Set.map ename . Set.fromList . elems

#if MIN_VERSION_Cabal(2,0,0)
       let installedData :: Set (FilePath, FilePath)
           installedData = Set.map (\ a -> (a, a)) $ Set.unions (Map.elems installedDataMap)
           installedExec :: Set String
#else
       let installedData = Set.map (\ a -> (a, a)) $ Set.unions (Map.elems installedDataMap)
#endif
           installedExec = Set.unions (Map.elems installedExecMap)

       prefixPath <- dataTop
       let dataFilePaths = Set.fromList (zip (List.map (prefixPath </>) (Cabal.dataFiles pkgDesc)) (Cabal.dataFiles pkgDesc)) :: Set (FilePath, FilePath)
#if MIN_VERSION_Cabal(2,0,0)
           execFilePaths :: Set FilePath
           execFilePaths = Set.map (unUnqualComponentName . Cabal.exeName) (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc))) :: Set FilePath
#else
           execFilePaths = Set.map Cabal.exeName (Set.filter (Cabal.buildable . Cabal.buildInfo) (Set.fromList (Cabal.executables pkgDesc))) :: Set FilePath
#endif
       let availableData = Set.union installedData dataFilePaths
           availableExec = Set.union installedExec execFilePaths

       use (A.debInfo . D.utilsPackageNameBase) >>= \ name ->
           case name of
             Nothing -> debianName B.Utilities hc >>= \ (BinPkgName name') -> (A.debInfo . D.utilsPackageNameBase) .= Just name'
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
         (A.debInfo . D.binaryDebDescription b . B.description) .?= Just desc
         -- This is really for all binary debs except the libraries - I'm not sure why
         (A.debInfo . D.rulesFragments) %= Set.insert (pack ("build" </> ppShow b ++ ":: build-ghc-stamp\n"))
         (A.debInfo . D.binaryDebDescription b . B.architecture) .?= Just (if Set.null utilsExec then All else Any)
         (A.debInfo . D.binaryDebDescription b . B.binarySection) .?= Just (MainSection "misc")
         binaryPackageRelations b B.Utilities
       -- Add the unassigned files to the utils packages
       Set.mapM_ (\ (foo, bar) -> (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallData b foo bar)) utilsDataMissing
       Set.mapM_ (\ name -> (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallCabalExec b name "usr/bin")) utilsExecMissing
    where
      ename i =
          case D.sourceDir i of
            (Nothing) -> D.execName i
            (Just s) ->  s </> D.execName i

expandAtoms :: Monad m => CabalT m ()
expandAtoms =
    do hc <- use (A.debInfo . D.flags . compilerFlavor)
       case hc of
         GHC -> (A.debInfo . D.flags . cabalFlagAssignments) %= (Set.union (Set.fromList (flagList "--ghc")))
#if MIN_VERSION_Cabal(1,22,0)
         GHCJS -> (A.debInfo . D.flags . cabalFlagAssignments) %= (Set.union (Set.fromList (flagList "--ghcjs")))
#endif
         x -> error $ "Sorry, compiler not supported: " ++ show x
       builddir <- use (A.debInfo . D.buildDir) >>= return . fromMaybe (case hc of
                                                               GHC -> "dist-ghc/build"
#if MIN_VERSION_Cabal(1,22,0)
                                                               GHCJS -> "dist-ghcjs/build"
#endif
                                                               _ -> error $ "Unexpected compiler: " ++ show hc)
       dDest <- dataDest
       expandApacheSites
       expandInstallCabalExecs builddir
       expandInstallCabalExecTo builddir
       expandInstallData dDest
       expandInstallTo
       expandFile
       expandWebsite
       expandServer
       expandBackups
       expandExecutable
    where
      expandApacheSites :: Monad m => CabalT m ()
      expandApacheSites =
          do mp <- get >>= return . view (A.debInfo . D.apacheSite)
             List.mapM_ expandApacheSite (Map.toList mp)
          where
            expandApacheSite (b, (dom, log, text)) =
                do (A.debInfo . D.atomSet) %= (Set.insert $ D.Link b ("/etc/apache2/sites-available/" ++ dom) ("/etc/apache2/sites-enabled/" ++ dom))
                   (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallDir b log)
                   (A.debInfo . D.atomSet) %= (Set.insert $ D.File b ("/etc/apache2/sites-available" </> dom) text)

      -- Turn A.InstallCabalExec into A.Install
      expandInstallCabalExecs :: Monad m => FilePath -> CabalT m ()
      expandInstallCabalExecs builddir = do
        hc <- use (A.debInfo . D.flags . compilerFlavor)
        use (A.debInfo . D.atomSet) >>= Set.mapM_ (doAtom hc)
          where
            doAtom :: Monad m => CompilerFlavor -> D.Atom -> CabalT m ()
            doAtom GHC (D.InstallCabalExec b name dest) = (A.debInfo . D.atomSet) %= (Set.insert $ D.Install b (builddir </> name </> name) dest)
#if MIN_VERSION_Cabal(1,22,0)
            -- A GHCJS executable is a directory with files, copy them
            -- all into place.
            doAtom GHCJS (D.InstallCabalExec b name dest) =
                (A.debInfo . D.rulesFragments) %= Set.insert
                     (Text.unlines
                        [ pack ("binary-fixup" </> ppShow b) <> "::"
                        , pack ("\t(cd " <> builddir </> name <> " && find " <> name <.> "jsexe" <> " -type f) |\\\n" <>
                                       "\t  while read i; do install -Dp " <> builddir </> name </> "$$i debian" </> ppShow b </> makeRelative "/" dest </> "$$i; done\n") ])
#endif
            doAtom _ _ = return ()

      -- Turn A.InstallCabalExecTo into a make rule
      expandInstallCabalExecTo :: Monad m => FilePath -> CabalT m ()
      expandInstallCabalExecTo builddir = do
        hc <- use (A.debInfo . D.flags . compilerFlavor)
        use (A.debInfo . D.atomSet) >>= Set.mapM_ (doAtom hc)
          where
            doAtom :: Monad m => CompilerFlavor -> D.Atom -> CabalT m ()
            doAtom GHC (D.InstallCabalExecTo b name dest) =
                (A.debInfo . D.rulesFragments) %= Set.insert
                                     (Text.unlines
                                       [ pack ("binary-fixup" </> ppShow b) <> "::"
                                       , "\tinstall -Dps " <> pack (builddir </> name </> name) <> " "
                                                           <> pack ("debian" </> ppShow b </> makeRelative "/" dest) ])
            doAtom hc (D.InstallCabalExecTo b name dest) = error $ "expandInstallCabalExecTo " ++ show hc ++ " " ++ show (D.InstallCabalExecTo b name dest)
            doAtom _ _ = return ()

      -- Turn A.InstallData into either an Install or an InstallTo
      expandInstallData :: Monad m => FilePath -> CabalT m ()
      expandInstallData dDest =
          use (A.debInfo . D.atomSet) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.InstallData b src dest) =
                if takeFileName src == takeFileName dest
                then (A.debInfo . D.atomSet) %= (Set.insert $ D.Install b src (dDest </> makeRelative "/" (takeDirectory dest)))
                else (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallTo b src (dDest </> makeRelative "/" dest))
            doAtom _ = return ()

      -- Turn A.InstallTo into a make rule
      expandInstallTo :: Monad m => CabalT m ()
      expandInstallTo =
          use (A.debInfo . D.atomSet) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.InstallTo b src dest) =
                (A.debInfo . D.rulesFragments) %= Set.insert
                                    (Text.unlines [ pack ("binary-fixup" </> ppShow b) <> "::"
                                                  , "\tinstall -Dp " <> pack src <> " " <> pack ("debian" </> ppShow b </> makeRelative "/" dest) ])
            doAtom _ = return ()

      -- Turn A.File into an intermediateFile and an A.Install
      expandFile :: Monad m => CabalT m ()
      expandFile =
          use (A.debInfo . D.atomSet) >>= List.mapM_ doAtom . Set.toList
          where
            doAtom :: Monad m => D.Atom -> CabalT m ()
            doAtom (D.File b path text) =
                do let (destDir', destName') = splitFileName path
                       tmpDir = "debian/cabalInstall" </> show (md5 (fromString (unpack text)))
                       tmpPath = tmpDir </> destName'
                   (A.debInfo . D.intermediateFiles) %= Set.insert (tmpPath, text)
                   (A.debInfo . D.atomSet) %= (Set.insert $ D.Install b tmpPath destDir')
            doAtom _ = return ()

      expandWebsite :: Monad m => CabalT m ()
      expandWebsite =
          do mp <- get >>= return . view (A.debInfo . D.website)
             pkgDesc <- use A.packageDescription
             List.mapM_ (\ (b, site) -> modify (siteAtoms pkgDesc b site)) (Map.toList mp)

      expandServer :: Monad m => CabalT m ()
      expandServer =
          do mp <- get >>= return . view (A.debInfo . D.serverInfo)
             pkgDesc <- use A.packageDescription
             List.mapM_ (\ (b, x) -> modify (serverAtoms pkgDesc b x False)) (Map.toList mp)

      expandBackups :: Monad m => CabalT m ()
      expandBackups =
          do mp <- get >>= return . view (A.debInfo . D.backups)
             List.mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)

      expandExecutable :: Monad m => CabalT m ()
      expandExecutable =
          do mp <- get >>= return . view (A.debInfo . D.executable)
             List.mapM_ (\ (b, f) -> modify (execAtoms b f)) (Map.toList mp)

-- | Add the normal default values to the rules files.
finalizeRules :: (MonadIO m, Functor m) => CabalT m ()
finalizeRules =
    do DebBase b <- debianNameBase
       hc <- use (A.debInfo . D.flags . compilerFlavor)
       let BinPkgName hcdeb = maybe (error "No compiler package") id (compilerPackageName hc B.Development)
       (A.debInfo . D.rulesHead) .?= Just "#!/usr/bin/make -f"
       (A.debInfo . D.rulesSettings) %= (++ ["DEB_CABAL_PACKAGE = " <> pack b])
       (A.debInfo . D.rulesSettings) %= (++ ["DEB_DEFAULT_COMPILER = " <> pack hcdeb])
       flags <- (flagString . Set.toList) <$> use (A.debInfo . D.flags . cabalFlagAssignments)
       unless (List.null flags) ((A.debInfo . D.rulesSettings) %= (++ ["DEB_SETUP_GHC6_CONFIGURE_ARGS = " <> pack flags]))
       (A.debInfo . D.rulesIncludes) %= (++ ["include /usr/share/cdbs/1/rules/debhelper.mk",
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
#if MIN_VERSION_Cabal(2,0,0)
  where tagWithValue ('-':name) = (mkFlagName (List.map toLower name), False)
        tagWithValue name       = (mkFlagName (List.map toLower name), True)
#else
  where tagWithValue ('-':name) = (FlagName (List.map toLower name), False)
        tagWithValue name       = (FlagName (List.map toLower name), True)
#endif

flagString :: [(FlagName, Bool)] -> String
#if MIN_VERSION_Cabal(2,0,0)
flagString = List.intercalate " " . List.map (\ (s, sense) -> "-f" ++ (if sense then "" else "-") ++ unFlagName s)
#else
flagString = List.intercalate " " . List.map (\ (FlagName s, sense) -> "-f" ++ (if sense then "" else "-") ++ s)
#endif
