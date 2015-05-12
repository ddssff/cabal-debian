-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Debianize.Files
    ( debianizationFileMap
    ) where


import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell, WriterT)
import Data.Char (isSpace)
import Data.List as List (dropWhile, dropWhileEnd, map)
import Data.Map as Map (fromListWithKey, insertWith, map, Map, mapKeys, toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Set as Set (fold, member, toList)
import Data.Text as Text (dropWhile, dropWhileEnd, intercalate, lines, null, pack, strip, Text, unlines, unpack)
import Debian.Control (Control'(Control, unControl), Field'(Field), Paragraph'(Paragraph))
import Debian.Control.Common ()
import qualified Debian.Debianize.DebInfo as D (Atom(Install, InstallDir, Link), atomSet, changelog, compat, control, copyright, installInit, intermediateFiles, logrotateStanza, postInst, postRm, preInst, preRm, rulesFragments, rulesHead, rulesIncludes, rulesSettings, sourceFormat, watch)
import Debian.Debianize.Monad (DebianT)
import Debian.Debianize.Prelude (showDeps')
import qualified Debian.Debianize.BinaryDebDescription as B (architecture, BinaryDebDescription, binaryPriority, binarySection, breaks, builtUsing, conflicts, depends, description, essential, package, PackageRelations, preDepends, provides, recommends, relations, replaces, suggests)
import Debian.Debianize.CopyrightDescription (CopyrightDescription)
import qualified Debian.Debianize.SourceDebDescription as S (binaryPackages, buildConflicts, buildConflictsIndep, buildDepends, buildDependsIndep, dmUploadAllowed, homepage, maintainer, priority, section, source, SourceDebDescription, standardsVersion, uploaders, vcsFields, VersionControlSpec(VCSArch, VCSBrowser, VCSBzr, VCSCvs, VCSDarcs, VCSGit, VCSHg, VCSMtn, VCSSvn), xDescription, XField(XField), XFieldDest(B, C, S), xFields)
import Debian.Pretty (PP(..), ppShow, prettyText, ppText, ppPrint)
import Debian.Relation (BinPkgName(BinPkgName), Relations)
import Distribution.PackageDescription (PackageDescription)
import Prelude hiding (dropWhile, init, log, unlines, writeFile)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (empty, Pretty(pPrint), text)

type FilesT m = WriterT [(FilePath, Text)] (DebianT m)

instance Pretty (PP Bool) where
    pPrint = text . show . unPP

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)

debianizationFileMap :: (Monad m, Functor m) => DebianT m (Map FilePath Text)
debianizationFileMap =
    fmap (Map.fromListWithKey (\ k a b -> error $ "Multiple values for " ++ k ++ ":\n  " ++ show a ++ "\n" ++ show b)) $ execWriterT $
    do -- here <- liftIO getCurrentDirectory
       tell =<< control
       tell =<< changelog
       tell =<< rules
       tell =<< compat
       tell =<< copyright
       tell =<< sourceFormatFiles
       tell =<< watchFile
       tell =<< installs
       tell =<< dirs
       tell =<< init
       tell =<< logrotate
       tell =<< links
       tell =<< postinstFiles
       tell =<< postrmFiles
       tell =<< preinstFiles
       tell =<< prermFiles
       tell =<< intermediates

sourceFormatFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
sourceFormatFiles = do
    fmt <- lift $ use D.sourceFormat
    return $ [("debian/source/format", pack . ppShow $ fmt)]

watchFile :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
watchFile = maybe [] (\ x -> [("debian/watch", x)]) <$> (lift $ use D.watch)

intermediates :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
intermediates = Set.toList <$> (lift $ use D.intermediateFiles)

installs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
installs =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ use (D.atomSet))
    where
      doAtom (D.Install b frm dst) mp = Map.insertWith (++) (pathf b) [pack (frm <> " " <> dst)] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".install"

dirs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
dirs =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ use D.atomSet)
    where
      doAtom (D.InstallDir b dir) mp = Map.insertWith (++) (pathf b) [pack dir] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".dirs"

init :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
init =
    (Map.toList . mapKeys pathf) <$> (lift $ use D.installInit)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
logrotate =
    (Map.toList . Map.map (\ stanzas -> Text.unlines (Set.toList stanzas)) . mapKeys pathf) <$> (lift $ use D.logrotateStanza)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
links =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ use D.atomSet)
    where
      doAtom (D.Link b loc t) mp = Map.insertWith (++) (pathf b) [pack loc <> " " <> pack t] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".links"

postinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postinstFiles =
     (Map.toList . mapKeys pathf) <$> (lift $ use D.postInst)
    where
      pathf (BinPkgName name) = "debian" </> name <> ".postinst"

postrmFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postrmFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ use D.postRm)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".postrm"

preinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
preinstFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ use D.preInst)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".preinst"

prermFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
prermFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ use D.preRm)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".prerm"

rules :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
rules =
    do Just rh <- lift (use (D.rulesHead))
       rassignments <- lift (use (D.rulesSettings)) >>= return . intercalate "\n"
       rincludes <- lift (use (D.rulesIncludes)) >>= return . intercalate "\n"
       rl <- (reverse . Set.toList) <$> lift (use (D.rulesFragments))
       return [("debian/rules", intercalate "\n\n" (filter (not . Text.null) (List.map strip (rh : rassignments : rincludes : rl))) <> "\n")]

changelog :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
changelog =
    do log <- lift $ use D.changelog
       return [("debian/changelog", pack (show (ppPrint (fromMaybe (error "No changelog in debianization") log))))]

control :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
control =
    do d <- lift $ use D.control
       return [("debian/control", prettyText (controlFile d))]

compat :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
compat =
    do t <- lift $ use D.compat
       return [("debian/compat", pack (show (fromMaybe (error "Missing DebCompat atom - is debhelper installed?") $ t) <> "\n"))]

copyright :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
copyright =
    do copyrt <- lift $ use (D.copyright)
       return $ maybe [] (\ x -> [("debian/copyright", prettyText x)]) copyrt

instance Pretty (PP (PackageDescription -> IO CopyrightDescription)) where
    pPrint _ = text "<function>"

controlFile :: S.SourceDebDescription -> Control' String
controlFile src =
    Control
    { unControl =
          (Paragraph
           ([Field ("Source", " " ++ (show . maybe empty ppPrint . view S.source $ src)),
             Field ("Maintainer", " " <> (show . maybe empty ppPrint . view S.maintainer $ src))] ++
            lField "Uploaders" (view S.uploaders src) ++
            (case view S.dmUploadAllowed src of True -> [Field ("DM-Upload-Allowed", " yes")]; False -> []) ++
            mField "Priority" (view S.priority src) ++
            mField "Section" (view S.section src) ++
            depField "Build-Depends" (view S.buildDepends src) ++
            depField "Build-Depends-Indep" (view S.buildDependsIndep src) ++
            depField "Build-Conflicts" (view S.buildConflicts src) ++
            depField "Build-Conflicts-Indep" (view S.buildConflictsIndep src) ++
            mField "Standards-Version" (view S.standardsVersion src) ++
            mField "Homepage" (view S.homepage src) ++
            List.map vcsField (Set.toList (view S.vcsFields src)) ++
            List.map xField (Set.toList (view S.xFields src)) ++
            mField "X-Description" (view S.xDescription src)) :
           List.map binary (view S.binaryPackages src))
    }
    where
      binary :: B.BinaryDebDescription -> Paragraph' String
      binary bin =
          Paragraph
           ([Field ("Package", " " ++ (show . ppPrint . view B.package $ bin)),
             Field ("Architecture", " " ++ (show . maybe empty ppPrint . view B.architecture $ bin))] ++
            mField "Section" (view B.binarySection bin) ++
            mField "Priority" (view B.binaryPriority bin) ++
            mField "Essential" (view B.essential bin) ++
            relFields (view B.relations bin) ++
            [Field ("Description", " " ++ (unpack . ensureDescription . fromMaybe mempty . view B.description $ bin))])
          where
            ensureDescription t =
                case List.dropWhileEnd Text.null (List.dropWhile Text.null (List.map (Text.dropWhileEnd isSpace) (Text.lines t))) of
                  [] -> "WARNING: No description available for package " <> ppText (view B.package bin)
                  (short : long) ->
                      Text.intercalate "\n"
                        ((if Text.null (Text.dropWhile isSpace short) then ("WARNING: No short description available for package " <> ppText (view B.package bin)) else short) : long)
      mField tag = maybe [] (\ x -> [Field (tag, " " <> (show . ppPrint $ x))])
      lField _ [] = []
      lField tag xs = [Field (tag, " " <> (show . ppPrint $ xs))]
      vcsField (S.VCSBrowser t) = Field ("Vcs-Browser", " " ++ unpack t)
      vcsField (S.VCSArch t) = Field ("Vcs-Arch", " " ++ unpack t)
      vcsField (S.VCSBzr t) = Field ("Vcs-Bzr", " " ++ unpack t)
      vcsField (S.VCSCvs t) = Field ("Vcs-Cvs", " " ++ unpack t)
      vcsField (S.VCSDarcs t) = Field ("Vcs-Darcs", " " ++ unpack t)
      vcsField (S.VCSGit t) = Field ("Vcs-Git", " " ++ unpack t)
      vcsField (S.VCSHg t) = Field ("Vcs-Hg", " " ++ unpack t)
      vcsField (S.VCSMtn t) = Field ("Vcs-Mtn", " " ++ unpack t)
      vcsField (S.VCSSvn t) = Field ("Vcs-Svn", " " ++ unpack t)
      xField (S.XField dests tag t) = Field (unpack ("X" <> showDests dests <> "-" <> tag), unpack (" " <> t))
      showDests s = if member S.B s then "B" else "" <>
                    if member S.S s then "S" else "" <>
                    if member S.C s then "C" else ""

relFields :: B.PackageRelations -> [Field' [Char]]
relFields rels =
    depField "Depends" (view B.depends rels) ++
    depField "Recommends" (view B.recommends rels) ++
    depField "Suggests" (view B.suggests rels) ++
    depField "Pre-Depends" (view B.preDepends rels) ++
    depField "Breaks" (view B.breaks rels) ++
    depField "Conflicts" (view B.conflicts rels) ++
    depField "Provides" (view B.provides rels) ++
    depField "Replaces" (view B.replaces rels) ++
    depField "Built-Using" (view B.builtUsing rels)

depField :: [Char] -> Relations -> [Field' [Char]]
depField tag rels = case rels of [] -> []; _ -> [Field (tag, " " ++ showDeps' rels)]
