-- | Read an existing Debianization from a directory file.
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.InputDebian
    ( inputDebianization
    , inputDebianizationFile
    , inputChangeLog
    , loadChangeLog
    , dataDest
    , dataTop
    ) where

import Debug.Trace
import Control.Lens
import Control.Monad (filterM)
import Control.Monad.State (put)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (isSpace)
import Data.Map as Map (insert, insertWith)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend)
#endif
import Data.Set as Set (fromList, insert, singleton)
import Data.Text (break, lines, null, pack, strip, Text, unpack, words)
import Data.Text.IO (readFile)
import Debian.Changes (ChangeLog, parseChangeLog)
import Debian.Control (Control'(unControl), ControlFunctions, Field, Field'(..), Paragraph'(..), parseControlFromFile, stripWS)
import Debian.Debianize.DebInfo (changelog, compat, control, copyright, install, installDir, installInit, intermediateFiles, link, logrotateStanza, postInst, postRm, preInst, preRm, rulesHead, sourceFormat, warning, watch)
import qualified Debian.Debianize.DebInfo as T (flags, makeDebInfo)
import Debian.Debianize.Monad (CabalT, DebianT)
import Debian.Debianize.CabalInfo (packageDescription)
import Debian.Debianize.BinaryDebDescription (BinaryDebDescription, newBinaryDebDescription)
import qualified Debian.Debianize.BinaryDebDescription as B (architecture, binaryPriority, multiArch, binarySection, breaks, builtUsing, conflicts, depends, description, essential, package, preDepends, provides, recommends, relations, replaces, suggests)
import Debian.Debianize.CopyrightDescription (readCopyrightDescription)
import Debian.Debianize.Prelude (getDirectoryContents', read', readFileMaybe, (.?=))
import qualified Debian.Debianize.SourceDebDescription as S (binaryPackages, buildConflicts, buildConflictsIndep, buildDepends, buildDependsIndep, dmUploadAllowed, homepage, newSourceDebDescription', priority, section, SourceDebDescription, standardsVersion, uploaders, xDescription, vcsFields, VersionControlSpec(VCSArch, VCSBrowser, VCSBzr, VCSCvs, VCSDarcs, VCSGit, VCSHg, VCSMtn, VCSSvn), XField(XField), xFields)
import Debian.Orphans ()
import Debian.Policy (parseMaintainer, parsePackageArchitectures, parseStandardsVersion, parseUploaders, readPriority, readSection, readMultiArch, readSourceFormat, Section(..))
import Debian.Relation (BinPkgName(..), parseRelations, Relations, SrcPkgName(..))
--import Debug.Trace (trace)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (PackageIdentifier(..), PackageName(..), unPackageName)
#else
import Distribution.Package (PackageIdentifier(..), PackageName(..))
#endif
import qualified Distribution.PackageDescription as Cabal (dataDir, PackageDescription(package))
import Prelude hiding (break, lines, log, null, readFile, sum, words)
import System.Directory (doesFileExist)
import System.FilePath ((</>), dropExtension, takeExtension)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
-- import System.Unix.Chroot (useEnv)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputDebianization :: MonadIO m => DebianT m ()
inputDebianization =
    do -- Erase any the existing information
       fs <- use T.flags
       put $ T.makeDebInfo fs
       (ctl, _) <- inputSourceDebDescription
       inputCabalInfoFromDirectory
       control .= ctl

-- | Try to input a file and if successful add it to the
-- debianization's list of "intermediate" files, files which will
-- simply be added to the final debianization without any
-- understanding of their contents or purpose.
inputDebianizationFile :: MonadIO m => FilePath -> DebianT m ()
inputDebianizationFile path =
    do inputCabalInfoFromDirectory
       liftIO (readFileMaybe path) >>= maybe (return ()) (\ text -> intermediateFiles %= Set.insert (path, text))

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
      src = set S.binaryPackages bins (S.newSourceDebDescription' findSource findMaint)
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
      readField (Field ("Standards-Version", value)) (desc, unrecognized) = (set S.standardsVersion (Just (parseStandardsVersion value)) desc, unrecognized)
      readField (Field ("Priority", value)) (desc, unrecognized) = (set S.priority (Just (readPriority value)) desc, unrecognized)
      readField (Field ("Section", value)) (desc, unrecognized) = (set S.section (Just (MainSection value)) desc, unrecognized)
      -- Optional
      readField (Field ("Homepage", value)) (desc, unrecognized) = (set S.homepage (Just (strip (pack value))) desc, unrecognized)
      readField (Field ("Uploaders", value)) (desc, unrecognized) = (set S.uploaders (either (const []) id (parseUploaders value)) desc, unrecognized)
      readField (Field ("DM-Upload-Allowed", value)) (desc, unrecognized) = (set S.dmUploadAllowed (yes value) desc, unrecognized)
      readField (Field ("Build-Depends", value)) (desc, unrecognized) = (set S.buildDepends (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts", value)) (desc, unrecognized) = (set S.buildConflicts (rels value) desc, unrecognized)
      readField (Field ("Build-Depends-Indep", value)) (desc, unrecognized) = (set S.buildDependsIndep (rels value) desc, unrecognized)
      readField (Field ("Build-Conflicts-Indep", value)) (desc, unrecognized) = (set S.buildConflictsIndep (rels value) desc, unrecognized)
      readField (Field ("Vcs-Browser", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSBrowser (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Arch", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSArch (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Bzr", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSBzr (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Cvs", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSCvs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Darcs", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSDarcs (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Git", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSGit (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Hg", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSHg (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Mtn", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSMtn (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("Vcs-Svn", s)) (desc, unrecognized) = (over S.vcsFields (\ vcsFields -> Set.insert (S.VCSSvn (pack s)) vcsFields) desc, unrecognized)
      readField (Field ("X-Description", value)) (desc, unrecognized) = (set S.xDescription (Just (pack value)) desc, unrecognized)
      readField field@(Field ('X' : fld, value)) (desc, unrecognized) =
          case span (`elem` "BCS") fld of
            (xs, '-' : more) -> (over S.xFields (\ xFields -> Set.insert (S.XField (fromList (map (read' (\ s -> error $ "parseSourceDebDescription: " ++ show s) . (: [])) xs)) (pack more) (pack value)) xFields) desc, unrecognized)
            _ -> (desc, field : unrecognized)
      readField field (desc, unrecognized) = (desc, field : unrecognized)

parseBinaryDebDescription :: Paragraph' String -> (BinaryDebDescription, [Field])
parseBinaryDebDescription (Paragraph fields) =
    foldr readField (bin, []) fields'
    where
      fields' = map stripField fields
      bin = set B.architecture (Just arch) (newBinaryDebDescription b)
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
      readField (Field ("Package", x)) (desc, unrecognized) = (set B.package (BinPkgName x) desc, unrecognized)
      readField (Field ("Architecture", x)) (desc, unrecognized) = (set B.architecture (Just (parsePackageArchitectures x)) desc, unrecognized)
      readField (Field ("Multi-Arch", x)) (desc, unrecognized) = (set B.multiArch (Just (readMultiArch x)) desc, unrecognized)
      readField (Field ("Section", x)) (desc, unrecognized) = (set B.binarySection (Just (readSection x)) desc, unrecognized)
      readField (Field ("Priority", x)) (desc, unrecognized) = (set B.binaryPriority (Just (readPriority x)) desc, unrecognized)
      readField (Field ("Essential", x)) (desc, unrecognized) = (set B.essential (Just (yes x)) desc, unrecognized)
      readField (Field ("Depends", x)) (desc, unrecognized) = (set (B.relations . B.depends) (rels x) desc, unrecognized)
      readField (Field ("Recommends", x)) (desc, unrecognized) = (set (B.relations . B.recommends) (rels x) desc, unrecognized)
      readField (Field ("Suggests", x)) (desc, unrecognized) = (set (B.relations . B.suggests) (rels x) desc, unrecognized)
      readField (Field ("Pre-Depends", x)) (desc, unrecognized) = (set (B.relations . B.preDepends) (rels x) desc, unrecognized)
      readField (Field ("Breaks", x)) (desc, unrecognized) = (set (B.relations . B.breaks) (rels x) desc, unrecognized)
      readField (Field ("Conflicts", x)) (desc, unrecognized) = (set (B.relations . B.conflicts) (rels x) desc, unrecognized)
      readField (Field ("Provides", x)) (desc, unrecognized) = (set (B.relations . B.provides) (rels x) desc, unrecognized)
      readField (Field ("Replaces", x)) (desc, unrecognized) = (set (B.relations . B.replaces) (rels x) desc, unrecognized)
      readField (Field ("Built-Using", x)) (desc, unrecognized) = (set (B.relations . B.builtUsing) (rels x) desc, unrecognized)
      readField (Field ("Description", x)) (desc, unrecognized) = (set B.description (Just (pack x)) desc, unrecognized)
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

-- | Look in several places for a debian changelog
inputChangeLog :: MonadIO m => DebianT m ()
inputChangeLog =
    do log <- liftIO loadChangeLog
       changelog .?= log

-- | Look in several places for a debian changelog
loadChangeLog :: IO (Maybe ChangeLog)
loadChangeLog =
    doPaths ["CHANGELOG", "ChangeLog", "changelog", "debian/changelog"]
    where
      doPaths :: [FilePath] -> IO (Maybe ChangeLog)
      doPaths (p : ps) = doPath p >>= maybe (doPaths ps) (\log -> {-putStrLn ("Found valid changelog at " ++ p) >>-} return (Just log))
      doPaths [] = return Nothing
      doPath :: FilePath -> IO (Maybe ChangeLog)
      doPath p = do
        t <- tryIOError (readFile p)
        either doExn doParse t
          where
            doParse :: Text -> IO (Maybe ChangeLog)
            doParse t = do
              return $ either (const Nothing) Just (parseChangeLog (unpack t))
            doExn :: IOError -> IO (Maybe ChangeLog)
            doExn e | isDoesNotExistError e = return Nothing
            doExn e = error ("inputChangelog: " ++ show e)

inputCabalInfoFromDirectory :: MonadIO m => DebianT m () -- .install files, .init files, etc.
inputCabalInfoFromDirectory =
    do findChangeLog -- Look for changelog in unconventional locations
       findFiles     -- If debian/changelog is found it will replace what we found above
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
      findChangeLog :: MonadIO m => DebianT m ()
      findChangeLog =
          filterM (liftIO . doesFileExist) ["changelog", "ChangeLog", "CHANGELOG"] >>= \names ->
          mapM_ (inputCabalInfo ".") names
      doFiles :: MonadIO m => FilePath -> DebianT m ()
      doFiles tmp =
          do sums <- liftIO $ getDirectoryContents' tmp `catchIOError` (\ _ -> return [])
             paths <- liftIO $ mapM (\ sum -> getDirectoryContents' (tmp </> sum) >>= return . map (sum </>)) sums >>= return . filter ((/= '~') . last) . concat
             files <- liftIO $ mapM (readFile . (tmp </>)) paths
             mapM_ (\ x -> intermediateFiles %= Set.insert x) (zip (map ("debian/cabalInstall" </>) paths) files)

-- | Construct a file path from the debian directory and a relative
-- path, read its contents and add the result to the debianization.
-- This may mean using a specialized parser from the debian package
-- (e.g. parseChangeLog), and some files (like control) are ignored
-- here, though I don't recall why at the moment.
inputCabalInfo :: MonadIO m => FilePath -> FilePath -> DebianT m ()
inputCabalInfo _ path | elem path ["control"] = return ()
inputCabalInfo debian name@"source/format" = liftIO (readFile (debian </> name)) >>= \ text -> either (\ x -> warning %= Set.insert x) ((sourceFormat .=)) (readSourceFormat text)
inputCabalInfo debian name@"watch" = liftIO (readFile (debian </> name)) >>= \ text -> watch .= Just text
inputCabalInfo debian name@"rules" = liftIO (readFile (debian </> name)) >>= \ text -> rulesHead .= (Just $ strip text <> pack "\n")
inputCabalInfo debian name@"compat" = liftIO (readFile (debian </> name)) >>= \ text -> compat .= Just (read' (\ s -> error $ "compat: " ++ show s) (unpack text))
inputCabalInfo debian name@"copyright" = liftIO (readFile (debian </> name)) >>= \ text -> copyright .= Just (readCopyrightDescription text)
-- The normal position for a debian changelog is debian/changelog, but
-- we also look for it in changelog, ChangeLog, and CHANGELOG because
-- hackage looks for it in those places and the debianization is
-- better off with those entries than without.
inputCabalInfo debian name@"changelog" = do
  log <- liftIO (readFile (debian </> name)) >>= return . either (const Nothing) Just . parseChangeLog . unpack
  changelog .= log
inputCabalInfo debian name =
    case (BinPkgName (dropExtension name), takeExtension name) of
      (p, ".install") ->   liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readInstall p) (lines text)
      (p, ".dirs") ->      liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readDir p) (lines text)
      (p, ".init") ->      liftIO (readFile (debian </> name)) >>= \ text -> installInit %= Map.insert p text
      (p, ".logrotate") -> liftIO (readFile (debian </> name)) >>= \ text -> logrotateStanza %= Map.insertWith mappend p (singleton text)
      (p, ".links") ->     liftIO (readFile (debian </> name)) >>= \ text -> mapM_ (readLink p) (lines text)
      (p, ".postinst") ->  liftIO (readFile (debian </> name)) >>= \ text -> postInst %= Map.insert p text
      (p, ".postrm") ->    liftIO (readFile (debian </> name)) >>= \ text -> postRm %= Map.insert p text
      (p, ".preinst") ->   liftIO (readFile (debian </> name)) >>= \ text -> preInst %= Map.insert p text
      (p, ".prerm") ->     liftIO (readFile (debian </> name)) >>= \ text -> preRm %= Map.insert p text
      (_, ".log") ->       return () -- Generated by debhelper
      (_, ".debhelper") -> return () -- Generated by debhelper
      (_, ".hs") ->        return () -- Code that uses this library
      (_, ".setup") ->     return () -- Compiled Setup.hs file
      (_, ".substvars") -> return () -- Unsupported
      (_, "") ->           return () -- File with no extension
      (_, x) | last x == '~' -> return () -- backup file
      _ -> liftIO (putStrLn $ "Ignored debianization file: " ++ debian </> name)

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

-- | Where to put the installed data files.  Computes the destination
-- directory from a Cabal package description.  This needs to match
-- the path cabal assigns to datadir in the
-- dist/build/autogen/Paths_packagename.hs module, or perhaps the path
-- in the CABAL_DEBIAN_DATADIR environment variable.
dataDest :: Monad m => CabalT m FilePath
dataDest = do
  d <- use packageDescription
#if MIN_VERSION_Cabal(2,0,0)
  return $ "usr/share" </> (unPackageName $ pkgName $ Cabal.package d)
#else
  return $ "usr/share" </> ((\ (PackageName x) -> x) $ pkgName $ Cabal.package d)

#endif
-- | Where to look for the data-files
dataTop :: Monad m => CabalT m FilePath
dataTop = do
  d <- use packageDescription
  return $ case Cabal.dataDir d of
             "" -> "."
             x -> x
