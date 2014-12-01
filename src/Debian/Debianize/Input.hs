-- | Read an existing Debianization from a directory file.
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Input
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , inputCabalization
    , inputCabalization'
    , inputMaintainer
    , dataDir
    ) where

import Debug.Trace (trace)

import Control.Applicative ((<$>))
import Control.Category ((.))
--import Control.DeepSeq (NFData, force)
import Control.Exception (bracket)
import Control.Monad (when, filterM)
import Control.Monad.State (get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace, toLower)
import Data.Lens.Lazy (getL, setL, modL, access)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as Set (Set, toList, fromList, insert, singleton)
import Data.Text (Text, unpack, pack, lines, words, break, strip, null)
import Data.Text.IO (readFile)
--import Data.Version (showVersion, Version(Version))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(logWho), parseChangeLog)
import Debian.Control (Control'(unControl), Paragraph'(..), stripWS, parseControlFromFile, Field, Field'(..), ControlFunctions)
import qualified Debian.Debianize.Types as T (maintainer, official)
import qualified Debian.Debianize.Types.Atoms as T (changelog, makeAtoms, compilerFlavors)
import Debian.Debianize.Types.BinaryDebDescription (BinaryDebDescription, newBinaryDebDescription)
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import Debian.Debianize.Types.CopyrightDescription (readCopyrightDescription, CopyrightDescription(..), FilesOrLicenseDescription(..))
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.Types.Atoms
    (control, warning, sourceFormat, watch, rulesHead, compat, packageDescription,
     copyright, changelog, installInit, postInst, postRm, preInst, preRm,
     logrotateStanza, link, install, installDir, intermediateFiles, cabalFlagAssignments, verbosity, buildEnv)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude (getDirectoryContents', readFileMaybe, read', intToVerbosity', (~=), (~?=), (+=), (++=), (+++=), (%=))
import Debian.Debianize.Types.Atoms (EnvSet(dependOS))
import Debian.GHC (newestAvailableCompilerId)
import Debian.Orphans ()
import Debian.Policy (Section(..), parseStandardsVersion, readPriority, readSection, parsePackageArchitectures, parseMaintainer,
                      parseUploaders, readSourceFormat, getDebianMaintainer, haskellMaintainer, fromCabalLicense)
import Debian.Relation (Relations, BinPkgName(..), SrcPkgName(..), parseRelations)
--import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerId)
import Distribution.Package (Package(packageId), PackageIdentifier(..), PackageName(PackageName), Dependency)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(maintainer, package, license, copyright {-, synopsis, description-}))
#if MIN_VERSION_Cabal(1,19,0)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFiles))
#else
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFile))
#endif
import Distribution.PackageDescription as Cabal (PackageDescription, FlagName)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (readFile, lines, words, break, null, log, sum, (.))
-- import qualified Prelude (lines)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Posix.Files (setFileCreationMask)
import System.Process (system)
import System.IO.Error (catchIOError, tryIOError)
-- import System.Unix.Chroot (useEnv)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputDebianization :: MonadIO m => EnvSet -> DebT m ()
inputDebianization envset =
    do -- Erase any the existing information
       let atoms = T.makeAtoms envset
       put atoms
       (ctl, _) <- inputSourceDebDescription
       inputAtomsFromDirectory
       control ~= ctl

-- | Try to input a file and if successful add it to the debianization.
inputDebianizationFile :: MonadIO m => FilePath -> DebT m ()
inputDebianizationFile path =
    do inputAtomsFromDirectory
       liftIO (readFileMaybe path) >>= maybe (return ()) (\ text -> intermediateFiles += (path, text))

inputSourceDebDescription :: MonadIO m => DebT m (S.SourceDebDescription, [Field])
inputSourceDebDescription =
    do paras <- liftIO $ parseControlFromFile "debian/control" >>= either (error . show) (return . unControl)
       case paras of
         [] -> error "Missing source paragraph"
         [_] -> error "Missing binary paragraph"
         (hd : tl) -> return $ parseSourceDebDescription hd tl

parseSourceDebDescription :: Paragraph' String -> [Paragraph' String] -> (S.SourceDebDescription, [Field])
parseSourceDebDescription (Paragraph fields) binaryParagraphs =
    foldr readField (src, []) fields'
    where
      fields' = map stripField fields
      src = setL S.binaryPackages bins (S.newSourceDebDescription' findSource findMaint)
      findSource = findMap "Source" SrcPkgName fields'
      findMaint = findMap "Maintainer" (\ m -> either (\ e -> error $ "Failed to parse maintainer field " ++ show m ++ ": " ++ show e) id . parseMaintainer $ m) fields'
      -- findStandards = findMap "Standards-Version" parseStandardsVersion fields'

      (bins, _extra) = unzip $ map parseBinaryDebDescription binaryParagraphs
      readField :: Field -> (S.SourceDebDescription, [Field]) -> (S.SourceDebDescription, [Field])
      -- Mandatory
      readField (Field ("Source", _)) x = x
      readField (Field ("Maintainer", _)) x = x
      -- readField (Field ("Standards-Version", _)) x = x
      -- Recommended
      readField (Field ("Standards-Version", value)) (desc, unrecognized) = (setL S.standardsVersion (Just (parseStandardsVersion value)) desc, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (setL S.priority (Just (readPriority value)) desc, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (setL S.section (Just (MainSection value)) desc, unrecognized)
      -- Optional
      readField (Field ("Homepage", value)) (desc, unrecognized) = (setL S.homepage (Just (strip (pack value))) desc, unrecognized)
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (setL S.uploaders (either (const []) id (parseUploaders value)) desc, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (setL S.dmUploadAllowed (yes value) desc, unrecognized)
      readField (Field ("Build-Depends", value)) (desc, unrecognized) = (setL S.buildDepends (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts", value)) (desc, unrecognized) = (setL S.buildConflicts (rels value) desc, unrecognized)
      readField (Field ("Build-Depends-Indep", value)) (desc, unrecognized) = (setL S.buildDependsIndep (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts-Indep", value)) (desc, unrecognized) = (setL S.buildConflictsIndep (rels value) desc, unrecognized)
      readField (Field ("Vcs-Browser", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSBrowser (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Arch", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSArch (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Bzr", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSBzr (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Cvs", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSCvs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Darcs", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSDarcs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Git", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSGit (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Hg", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSHg (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Mtn", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSMtn (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Svn", s)) (desc, unrecognized) = (modL S.vcsFields (\ vcsFields -> insert (S.VCSSvn (pack s)) vcsFields) desc, unrecognized)
      readField field@(Field ('X' : fld, value)) (desc, unrecognized) =
          case span (`elem` "BCS") fld of
            (xs, '-' : more) -> (modL S.xFields (\ xFields -> insert (S.XField (fromList (map (read' (\ s -> error $ "parseSourceDebDescription: " ++ show s) . (: [])) xs)) (pack more) (pack value)) xFields) desc, unrecognized)
            _ -> (desc, field : unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

parseBinaryDebDescription :: Paragraph' String -> (BinaryDebDescription, [Field])
parseBinaryDebDescription (Paragraph fields) =
    foldr readField (bin, []) fields'
    where
      fields' = map stripField fields
      bin = setL B.architecture (Just arch) (newBinaryDebDescription b)
      b :: BinPkgName
      b = findMap "Package" BinPkgName fields'
      arch = findMap "Architecture" parsePackageArchitectures fields'
{-
(BinPkgName (fromJust (fieldValue "Package" bin)))
(read' (fromJust (fieldValue "Architecture" bin)))
, []
    foldr readField (newBinaryDebDescription (BinPkgName (fromJust (fieldValue "Package" bin))) (read' (fromJust (fieldValue "Architecture" bin))), []) (map stripField fields)
-}

      readField :: Field -> (BinaryDebDescription, [Field]) -> (BinaryDebDescription, [Field])
      readField (Field ("Package", x)) (desc, unrecognized) = (setL B.package (BinPkgName x) desc, unrecognized)
      readField (Field ("Architecture", x)) (desc, unrecognized) = (setL B.architecture (Just (parsePackageArchitectures x)) desc, unrecognized)
      readField (Field ("Section", x)) (desc, unrecognized) = (setL B.binarySection (Just (readSection x)) desc, unrecognized)
      readField (Field ("Priority", x)) (desc, unrecognized) = (setL B.binaryPriority (Just (readPriority x)) desc, unrecognized)
      readField (Field ("Essential", x)) (desc, unrecognized) = (setL B.essential (Just (yes x)) desc, unrecognized)
      readField (Field ("Depends", x)) (desc, unrecognized) = (setL (B.depends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Recommends", x)) (desc, unrecognized) = (setL (B.recommends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Suggests", x)) (desc, unrecognized) = (setL (B.suggests . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Pre-Depends", x)) (desc, unrecognized) = (setL (B.preDepends . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Breaks", x)) (desc, unrecognized) = (setL (B.breaks . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Conflicts", x)) (desc, unrecognized) = (setL (B.conflicts . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Provides", x)) (desc, unrecognized) = (setL (B.provides . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Replaces", x)) (desc, unrecognized) = (setL (B.replaces . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Built-Using", x)) (desc, unrecognized) = (setL (B.builtUsing . B.relations) (rels x) desc, unrecognized)
      readField (Field ("Description", x)) (desc, unrecognized) = (setL B.description (Just (pack x)) desc, unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

-- | Look for a field and apply a function to its value
findMap :: String -> (String -> a) -> [Field] -> a
findMap field f fields =
    fromMaybe (error $ "Missing " ++ show field ++ " field in " ++ show fields) (foldr findMap' Nothing fields)
    where
      findMap' (Field (fld, val)) x = if fld == field then Just (f val) else x
      findMap' _ x = x

stripField :: ControlFunctions a => Field' a -> Field' a
stripField (Field (a, b)) = Field (a, stripWS b)
stripField x = x

rels :: String -> Relations
rels s =
    either (\ e -> error ("Relations field error: " ++ show e ++ "\n  " ++ s)) id (parseRelations s)

yes :: String -> Bool
yes "yes" = True
yes "no" = False
yes x = error $ "Expecting yes or no: " ++ x

inputChangeLog :: MonadIO m => DebT m ()
inputChangeLog =
    do log <- liftIO $ tryIOError (readFile "debian/changelog" >>= return . parseChangeLog . unpack)
       changelog ~?= either (\ _ -> Nothing) Just log

inputAtomsFromDirectory :: MonadIO m => DebT m () -- .install files, .init files, etc.
inputAtomsFromDirectory =
    do findFiles
       doFiles ("./debian/cabalInstall")
    where
      -- Find regular files in the debian/ or in debian/source/format/ and
      -- add them to the debianization.
      findFiles :: MonadIO m => DebT m ()
      findFiles =
          liftIO (getDirectoryContents' ("debian")) >>=
          return . (++ ["source/format"]) >>=
          liftIO . filterM (doesFileExist . (("debian") </>)) >>= \ names ->
          mapM_ (inputAtoms ("debian")) names
      doFiles :: MonadIO m => FilePath -> DebT m ()
      doFiles tmp =
          do sums <- liftIO $ getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- liftIO $ mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . filter ((/= '~') . last) . concat
             files <- liftIO $ mapM (readFile . (tmp </>)) paths
             mapM_ (intermediateFiles +=) (zip (map ("debian/cabalInstall" </>) paths) files)

-- | Construct a file path from the debian directory and a relative
-- path, read its contents and add the result to the debianization.
-- This may mean using a specialized parser from the debian package
-- (e.g. parseChangeLog), and some files (like control) are ignored
-- here, though I don't recall why at the moment.
inputAtoms :: MonadIO m => FilePath -> FilePath -> DebT m ()
inputAtoms _ path | elem path ["control"] = return ()
inputAtoms debian name@"source/format" = liftIO (readFile (debian </> name)) >>= \ text -> either (warning +=) ((sourceFormat ~=) . Just) (readSourceFormat text)
inputAtoms debian name@"watch" = liftIO (readFile (debian </> name)) >>= \ text -> watch ~= Just text
inputAtoms debian name@"rules" = liftIO (readFile (debian </> name)) >>= \ text -> rulesHead ~= (Just text)
inputAtoms debian name@"compat" = liftIO (readFile (debian </> name)) >>= \ text -> compat ~= Just (read' (\ s -> error $ "compat: " ++ show s) (unpack text))
inputAtoms debian name@"copyright" = liftIO (readFile (debian </> name)) >>= \ text -> copyright ~= readCopyrightDescription text
inputAtoms debian name@"changelog" =
    liftIO (readFile (debian </> name)) >>= return . parseChangeLog . unpack >>= \ log -> changelog ~= Just log
inputAtoms debian name =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readInstall p) (lines text)
      (p, ".dirs") ->      liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readDir p) (lines text)
      (p, ".init") ->      liftIO (readFile (debian </> name)) >>= \ text -> installInit ++= (p, text)
      (p, ".logrotate") -> liftIO (readFile (debian </> name)) >>= \ text -> logrotateStanza +++= (p, singleton text)
      (p, ".links") ->     liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readLink p) (lines text)
      (p, ".postinst") ->  liftIO (readFile (debian </> name)) >>= \ text -> postInst ++= (p, text)
      (p, ".postrm") ->    liftIO (readFile (debian </> name)) >>= \ text -> postRm ++= (p, text)
      (p, ".preinst") ->   liftIO (readFile (debian </> name)) >>= \ text -> preInst ++= (p, text)
      (p, ".prerm") ->     liftIO (readFile (debian </> name)) >>= \ text -> preRm ++= (p, text)
      (_, ".log") ->       return () -- Generated by debhelper
      (_, ".debhelper") -> return () -- Generated by debhelper
      (_, ".hs") ->        return () -- Code that uses this library
      (_, ".setup") ->     return () -- Compiled Setup.hs file
      (_, ".substvars") -> return () -- Unsupported
      (_, "") ->           return () -- File with no extension
      (_, x) | last x == '~' -> return () -- backup file
      _ -> trace ("Ignored: " ++ debian </> name) (return ())

-- | Read a line from a debian .links file
readLink :: Monad m => BinPkgName -> Text -> DebT m ()
readLink p line =
    case words line of
      [a, b] -> link p (unpack a) (unpack b)
      [] -> return ()
      _ -> trace ("Unexpected value passed to readLink: " ++ show line) (return ())

-- | Read a line from a debian .install file
readInstall :: Monad m => BinPkgName -> Text -> DebT m ()
readInstall p line =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> install p (unpack (strip a)) (unpack (strip b))

-- | Read a line from a debian .dirs file
readDir :: Monad m => BinPkgName -> Text -> DebT m ()
readDir p line = installDir p (unpack line)

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

inputCabalization :: (MonadIO m, Functor m) => DebT m ()
inputCabalization =
    do vb <- access verbosity >>= return . intToVerbosity'
       flags <- access cabalFlagAssignments
       root <- dependOS <$> access buildEnv
       hcs <- access T.compilerFlavors
       let cids = map (newestAvailableCompilerId root) (toList hcs)
       ePkgDescs <- liftIO $ inputCabalization' vb flags cids
       mapM_ (either (\ deps -> liftIO getCurrentDirectory >>= \ here ->
                                error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
                     (\ pkgDesc -> do
                        packageDescription ~= Just pkgDesc
                        -- This will contain either the contents of the file given in
                        -- the license-file: field or the contents of the license:
                        -- field.
                        licenseFiles <- mapM (\ path -> liftIO (readFileMaybe path) >>= \ text -> return (path, text))
#if MIN_VERSION_Cabal(1,19,0)
                                             (Cabal.licenseFiles pkgDesc)
#else
                                             (case Cabal.licenseFile pkgDesc of
                                                "" -> []
                                                path -> [path])
#endif
                        -- It is possible we might interpret the license file path
                        -- as a license name, so I hang on to it here.
                        let licenseFiles' = mapMaybe (\ (path, text) -> maybe Nothing (\ t -> Just (path, t)) text) licenseFiles
                        copyright %= cabalToCopyrightDescription pkgDesc licenseFiles'
                      ))
             ePkgDescs

cabalToCopyrightDescription :: PackageDescription -> [(FilePath, Text)] -> CopyrightDescription -> CopyrightDescription
cabalToCopyrightDescription pkgDesc licenseFiles cdesc =
    let triples = zip3 (repeat (nothingIf (null . strip) (pack (Cabal.copyright pkgDesc))))
                       (repeat (Cabal.license pkgDesc))
                       (case licenseFiles of
                          [] -> [Nothing]
                          xs -> map (Just. snd) xs)
        fnls = map (\ (copyrt, license, comment) ->
                         FilesDescription
                                {_filesPattern = "*"
                                , _filesCopyright = fromMaybe (pack "(No copyright field in cabal file)") copyrt
                                , _filesLicense = fromCabalLicense license
                                , _filesComment = comment }) triples in
     cdesc { _filesAndLicenses = fnls }

inputCabalization' :: Verbosity -> Set (FlagName, Bool) -> [CompilerId] -> IO [Either [Dependency] PackageDescription]
inputCabalization' vb flags cids = do
         descPath <- defaultPackageDesc vb
         genPkgDesc <- readPackageDescription vb descPath
         let finalized = map (\ cid -> finalizePackageDescription (toList flags) (const True) (Platform buildArch buildOS) cid [] genPkgDesc) cids
         mapM (either (return . Left)
                      (\ (pkgDesc, _) -> do bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                                            return (Right pkgDesc)))
              finalized

-- | Run the package's configuration script.
autoreconf :: Verbosity -> Cabal.PackageDescription -> IO ()
autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

-- chroot :: NFData a => FilePath -> IO a -> IO a
-- chroot "/" task = task
-- chroot root task = useEnv root (return . force) task

-- | Try to compute a string for the the debian "Maintainer:" field using, in this order
--    1. the Debian Haskell Group, @pkg-haskell-maintainers\@lists.alioth.debian.org@,
--       if --official is set
--    2. the maintainer explicitly specified using "Debian.Debianize.Monad.maintainer"
--    3. the maintainer field of the cabal package, but only if --official is not set,
--    4. the value returned by getDebianMaintainer, which looks in several environment variables,
--    5. the signature from the latest entry in debian/changelog,
--    6. the Debian Haskell Group, @pkg-haskell-maintainers\@lists.alioth.debian.org@
inputMaintainer :: MonadIO m => DebT m ()
inputMaintainer =
    do Just pkgDesc <- access packageDescription
       let cabalMaintainer = case Cabal.maintainer pkgDesc of
                               "" -> Nothing
                               x -> either (const Nothing) Just (parseMaintainer (takeWhile (\ c -> c /= ',' && c /= '\n') x))
       o <- access T.official
       when o $ T.maintainer ~?= Just haskellMaintainer

       T.maintainer ~?= cabalMaintainer
       debianMaintainer <- liftIO getDebianMaintainer
       T.maintainer ~?= debianMaintainer
       changelogMaintainer <-
           do log <- get >>= return . getL T.changelog
              case log of
                Just (ChangeLog (entry : _)) ->
                    case (parseMaintainer (logWho entry)) of
                      Left _e -> return $ Nothing -- Just $ NameAddr (Just "Invalid signature in changelog") (show e)
                      Right x -> return (Just x)
                _ -> return Nothing
       T.maintainer ~?= changelogMaintainer

-- | Compute the Cabal data directory for a Linux install from a Cabal
-- package description.  This needs to match the path cabal assigns to
-- datadir in the dist/build/autogen/Paths_packagename.hs module, or
-- perhaps the path in the cabal_debian_datadir environment variable.
dataDir :: Cabal.PackageDescription -> FilePath
dataDir p =
    let PackageName pkgname = pkgName . Cabal.package $ p in
    "usr/share" </> map toLower pkgname
