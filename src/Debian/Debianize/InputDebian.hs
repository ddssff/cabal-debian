-- | Read an existing Debianization from a directory file.
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.InputDebian
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , dataDir
    ) where

import Control.Category ((.))
import Control.Monad (filterM)
import Control.Monad.State (put)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (isSpace)
import Data.Lens.Lazy (access, modL, setL)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, insert, singleton)
import Data.Text (break, lines, null, pack, strip, Text, unpack, words)
import Data.Text.IO (readFile)
import Debian.Changes (parseChangeLog)
import Debian.Control (Control'(unControl), ControlFunctions, Field, Field'(..), Paragraph'(..), parseControlFromFile, stripWS)
import Debian.Debianize.DebInfo (changelog, compat, control, copyright, install, installDir, installInit, intermediateFiles, link, logrotateStanza, postInst, postRm, preInst, preRm, rulesHead, sourceFormat, warning, watch)
import qualified Debian.Debianize.DebInfo as T (flags, makeDebInfo)
import Debian.Debianize.Monad (CabalT, DebianT)
import Debian.Debianize.Prelude ((+++=), (++=), (+=), getDirectoryContents', read', readFileMaybe, (~=), (~?=))
import Debian.Debianize.CabalInfo (packageDescription)
import Debian.Debianize.BinaryDebDescription (BinaryDebDescription, newBinaryDebDescription)
import qualified Debian.Debianize.BinaryDebDescription as B (architecture, binaryPriority, binarySection, breaks, builtUsing, conflicts, depends, description, essential, package, preDepends, provides, recommends, relations, replaces, suggests)
import Debian.Debianize.CopyrightDescription (readCopyrightDescription)
import qualified Debian.Debianize.SourceDebDescription as S (binaryPackages, buildConflicts, buildConflictsIndep, buildDepends, buildDependsIndep, dmUploadAllowed, homepage, newSourceDebDescription', priority, section, SourceDebDescription, standardsVersion, uploaders, vcsFields, VersionControlSpec(VCSArch, VCSBrowser, VCSBzr, VCSCvs, VCSDarcs, VCSGit, VCSHg, VCSMtn, VCSSvn), XField(XField), xFields)
import Debian.Orphans ()
import Debian.Policy (parseMaintainer, parsePackageArchitectures, parseStandardsVersion, parseUploaders, readPriority, readSection, readSourceFormat, Section(..))
import Debian.Relation (BinPkgName(..), parseRelations, Relations, SrcPkgName(..))
import Debug.Trace (trace)
import Distribution.Package (PackageIdentifier(..), PackageName(unPackageName))
import qualified Distribution.PackageDescription as Cabal (dataDir, PackageDescription(package))
import Prelude hiding ((.), break, lines, log, null, readFile, sum, words)
import System.Directory (doesFileExist)
import System.FilePath ((</>), dropExtension, takeExtension)
import System.IO.Error (catchIOError, tryIOError)
-- import System.Unix.Chroot (useEnv)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputDebianization :: MonadIO m => DebianT m ()
inputDebianization =
    do -- Erase any the existing information
       fs <- access T.flags
       put $ T.makeDebInfo fs
       (ctl, _) <- inputSourceDebDescription
       inputCabalInfoFromDirectory
       control ~= ctl

-- | Try to input a file and if successful add it to the debianization.
inputDebianizationFile :: MonadIO m => FilePath -> DebianT m ()
inputDebianizationFile path =
    do inputCabalInfoFromDirectory
       liftIO (readFileMaybe path) >>= maybe (return ()) (\ text -> intermediateFiles += (path, text))

inputSourceDebDescription :: MonadIO m => DebianT m (S.SourceDebDescription, [Field])
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

inputChangeLog :: MonadIO m => DebianT m ()
inputChangeLog =
    do log <- liftIO $ tryIOError (readFile "debian/changelog" >>= return . parseChangeLog . unpack)
       changelog ~?= either (\ _ -> Nothing) Just log

inputCabalInfoFromDirectory :: MonadIO m => DebianT m () -- .install files, .init files, etc.
inputCabalInfoFromDirectory =
    do findFiles
       doFiles ("./debian/cabalInstall")
    where
      -- Find regular files in the debian/ or in debian/source/format/ and
      -- add them to the debianization.
      findFiles :: MonadIO m => DebianT m ()
      findFiles =
          liftIO (getDirectoryContents' ("debian")) >>=
          return . (++ ["source/format"]) >>=
          liftIO . filterM (doesFileExist . (("debian") </>)) >>= \ names ->
          mapM_ (inputCabalInfo ("debian")) names
      doFiles :: MonadIO m => FilePath -> DebianT m ()
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
inputCabalInfo :: MonadIO m => FilePath -> FilePath -> DebianT m ()
inputCabalInfo _ path | elem path ["control"] = return ()
inputCabalInfo debian name@"source/format" = liftIO (readFile (debian </> name)) >>= \ text -> either (warning +=) ((sourceFormat ~=) . Just) (readSourceFormat text)
inputCabalInfo debian name@"watch" = liftIO (readFile (debian </> name)) >>= \ text -> watch ~= Just text
inputCabalInfo debian name@"rules" = liftIO (readFile (debian </> name)) >>= \ text -> rulesHead ~= (Just $ strip text <> "\n")
inputCabalInfo debian name@"compat" = liftIO (readFile (debian </> name)) >>= \ text -> compat ~= Just (read' (\ s -> error $ "compat: " ++ show s) (unpack text))
inputCabalInfo debian name@"copyright" = liftIO (readFile (debian </> name)) >>= \ text -> copyright ~= (\ _ -> return (readCopyrightDescription text))
inputCabalInfo debian name@"changelog" =
    liftIO (readFile (debian </> name)) >>= return . parseChangeLog . unpack >>= \ log -> changelog ~= Just log
inputCabalInfo debian name =
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
readLink :: Monad m => BinPkgName -> Text -> DebianT m ()
readLink p line =
    case words line of
      [a, b] -> link p (unpack a) (unpack b)
      [] -> return ()
      _ -> trace ("Unexpected value passed to readLink: " ++ show line) (return ())

-- | Read a line from a debian .install file
readInstall :: Monad m => BinPkgName -> Text -> DebianT m ()
readInstall p line =
    case break isSpace line of
      (_, b) | null b -> error $ "readInstall: syntax error in .install file for " ++ show p ++ ": " ++ show line
      (a, b) -> install p (unpack (strip a)) (unpack (strip b))

-- | Read a line from a debian .dirs file
readDir :: Monad m => BinPkgName -> Text -> DebianT m ()
readDir p line = installDir p (unpack line)

-- chroot :: NFData a => FilePath -> IO a -> IO a
-- chroot "/" task = task
-- chroot root task = useEnv root (return . force) task

-- | Compute the Cabal data directory for a Linux install from a Cabal
-- package description.  This needs to match the path cabal assigns to
-- datadir in the dist/build/autogen/Paths_packagename.hs module, or
-- perhaps the path in the cabal_debian_datadir environment variable.
dataDir :: MonadIO m => CabalT m FilePath
dataDir = do
  d <- access packageDescription
  return $ case Cabal.dataDir d of
             [] -> "usr/share" </> (unPackageName $ pkgName $ Cabal.package d)
             x -> x
