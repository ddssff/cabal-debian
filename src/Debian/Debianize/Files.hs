-- | Convert a Debianization into a list of files that can then be
-- written out.
{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.Files
    ( debianizationFileMap
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Debian.Control.Common ()
import Data.Lens.Lazy (access, getL)
import Data.List as List (map, dropWhileEnd)
import Data.Map as Map (Map, map, toList, fromListWithKey, mapKeys, insertWith)
import Data.Maybe
import Data.Monoid ((<>), mempty)
import Data.Set as Set (toList, member, fold)
import Data.Text as Text (Text, pack, unpack, lines, unlines, strip, null, intercalate)
import Debian.Control (Control'(Control, unControl), Paragraph'(Paragraph), Field'(Field))
import Debian.Debianize.Goodies (makeRulesHead)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude (showDeps')
import qualified Debian.Debianize.Types.Atoms as T
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Pretty (PP(..), ppDisplay, ppPrint, ppDisplay')
import Debian.Relation (Relations, BinPkgName(BinPkgName))
import Distribution.License (License(AllRightsReserved))
import Prelude hiding (init, unlines, writeFile, log)
--import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text, empty)

type FilesT m = WriterT [(FilePath, Text)] (DebT m)

instance Pretty (PP Bool) where
    pPrint = text . show . unPP

-- | Turn the Debianization into a list of files, making sure the text
-- associated with each path is unique.  Assumes that
-- finalizeDebianization has already been called.  (Yes, I'm
-- considering building one into the other, but it is handy to look at
-- the Debianization produced by finalizeDebianization in the unit
-- tests.)

debianizationFileMap :: (Monad m, Functor m) => DebT m (Map FilePath Text)
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
sourceFormatFiles =
    maybe [] (\ x -> [("debian/source/format", pack (ppDisplay x))]) <$> (lift $ access T.sourceFormat)

watchFile :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
watchFile = maybe [] (\ x -> [("debian/watch", x)]) <$> (lift $ access T.watch)

intermediates :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
intermediates = Set.toList <$> (lift $ access T.intermediateFiles)

installs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
installs =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ access T.atomSet)
    where
      doAtom (T.Install b from dest) mp = Map.insertWith (++) (pathf b) [pack (from <> " " <> dest)] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".install"

dirs :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
dirs =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ access T.atomSet)
    where
      doAtom (T.InstallDir b dir) mp = Map.insertWith (++) (pathf b) [pack dir] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".dirs"

init :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
init =
    (Map.toList . mapKeys pathf) <$> (lift $ access T.installInit)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".init"

-- FIXME - use a map and insertWith, check for multiple entries
logrotate :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
logrotate =
    (Map.toList . Map.map (\ stanzas -> Text.unlines (Set.toList stanzas)) . mapKeys pathf) <$> (lift $ access T.logrotateStanza)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".logrotate"

-- | Assemble all the links by package and output one file each
links :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
links =
    (Map.toList . Map.map unlines . Set.fold doAtom mempty) <$> (lift $ access T.atomSet)
    where
      doAtom (T.Link b loc t) mp = Map.insertWith (++) (pathf b) [pack loc <> " " <> pack t] mp
      doAtom _ mp = mp
      pathf name = "debian" </> show (ppPrint name) ++ ".links"

postinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postinstFiles =
     (Map.toList . mapKeys pathf) <$> (lift $ access T.postInst)
    where
      pathf (BinPkgName name) = "debian" </> name <> ".postinst"

postrmFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
postrmFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access T.postRm)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".postrm"

preinstFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
preinstFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access T.preInst)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".preinst"

prermFiles :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
prermFiles =
    (Map.toList . mapKeys pathf) <$> (lift $ access T.preRm)
    where
      pathf name = "debian" </> show (ppPrint name) ++ ".prerm"

rules :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
rules =
    do rh <- lift (access T.rulesHead) >>= maybe (lift makeRulesHead) return
       rl <- (reverse . Set.toList) <$> lift (access T.rulesFragments)
       return [("debian/rules", Text.unlines (rh : rl))]

changelog :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
changelog =
    do log <- lift $ access T.changelog
       return [("debian/changelog", pack (show (ppPrint (fromMaybe (error "No changelog in debianization") log))))]

control :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
control =
    do d <- lift $ access T.control
       return [("debian/control", ppDisplay' (controlFile d))]

compat :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
compat =
    do t <- lift $ access T.compat
       return [("debian/compat", pack (show (fromMaybe (error "Missing DebCompat atom - is debhelper installed?") $ t) <> "\n"))]

copyright :: (Monad m, Functor m) => FilesT m [(FilePath, Text)]
copyright =
    do copyrt <- lift $ access T.copyright
       license <- lift $ access T.license
       licenseFile <- lift $ access T.licenseFile
       return [("debian/copyright", case (licenseFile, copyrt, license) of
                                      (Just x, _, _) -> x <> "\n"
                                      (_, Just x, y) -> x <> "\n" <> maybe mempty (\ z -> pack ("License: " <> (show  (ppPrint z)) <> "\n")) y
                                      (_, _, Just x) -> pack ("License: " <> show (ppPrint x) <> "\n")
                                      _ -> pack ("License: " <> show (ppPrint AllRightsReserved)))]

controlFile :: S.SourceDebDescription -> Control' String
controlFile src =
    Control
    { unControl =
          (Paragraph
           ([Field ("Source", " " ++ (show . maybe empty ppPrint . getL S.source $ src)),
             Field ("Maintainer", " " <> (show . maybe empty ppPrint . getL S.maintainer $ src))] ++
            lField "Uploaders" (getL S.uploaders src) ++
            (case getL S.dmUploadAllowed src of True -> [Field ("DM-Upload-Allowed", " yes")]; False -> []) ++
            mField "Priority" (getL S.priority src) ++
            mField "Section" (getL S.section src) ++
            depField "Build-Depends" (getL S.buildDepends src) ++
            depField "Build-Depends-Indep" (getL S.buildDependsIndep src) ++
            depField "Build-Conflicts" (getL S.buildConflicts src) ++
            depField "Build-Conflicts-Indep" (getL S.buildConflictsIndep src) ++
            mField "Standards-Version" (getL S.standardsVersion src) ++
            mField "Homepage" (getL S.homepage src) ++
            mField "X-Description" (getL S.xDescription src) ++
            List.map vcsField (Set.toList (getL S.vcsFields src)) ++
            List.map xField (Set.toList (getL S.xFields src))) :
           List.map binary (getL S.binaryPackages src))
    }
    where
      binary :: B.BinaryDebDescription -> Paragraph' String
      binary bin =
          Paragraph
           ([Field ("Package", " " ++ (show . ppPrint . getL B.package $ bin)),
             Field ("Architecture", " " ++ (show . maybe empty ppPrint . getL B.architecture $ bin))] ++
            mField "Section" (getL B.binarySection bin) ++
            mField "Priority" (getL B.binaryPriority bin) ++
            mField "Essential" (getL B.essential bin) ++
            relFields (getL B.relations bin) ++
            [Field ("Description", " " ++ (unpack . ensureDescription . fromMaybe mempty . getL B.description $ bin))])
          where
            ensureDescription t =
                case dropWhileEnd Text.null (dropWhile Text.null (List.map Text.strip (Text.lines t))) of
                  [] -> "WARNING: No description available for package " <> ppDisplay' (getL B.package bin)
                  (short : long) ->
                      Text.intercalate "\n"
                        ((if Text.null (strip short) then ("WARNING: No short description available for package " <> ppDisplay' (getL B.package bin)) else short) : long)
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
    depField "Depends" (getL B.depends rels) ++
    depField "Recommends" (getL B.recommends rels) ++
    depField "Suggests" (getL B.suggests rels) ++
    depField "Pre-Depends" (getL B.preDepends rels) ++
    depField "Breaks" (getL B.breaks rels) ++
    depField "Conflicts" (getL B.conflicts rels) ++
    depField "Provides" (getL B.provides rels) ++
    depField "Replaces" (getL B.replaces rels) ++
    depField "Built-Using" (getL B.builtUsing rels)

depField :: [Char] -> Relations -> [Field' [Char]]
depField tag rels = case rels of [] -> []; _ -> [Field (tag, " " ++ showDeps' (tag ++ ":") rels)]
