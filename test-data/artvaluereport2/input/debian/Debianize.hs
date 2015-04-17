{-# LANGUAGE CPP, OverloadedStrings #-}


import Control.Lens.Extended
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (singleton, insert)
import Data.Text as Text (intercalate)
import Debian.Changes (ChangeLog(..))
import Debian.Debianize -- (debianize, doBackups, doExecutable, doServer, doWebsite, inputChangeLog, inputDebianization, debianDefaultAtoms)
-- import Debian.Debianize.BasicFlags (newFlags)
-- import qualified Debian.Debianize.Atoms as A
-- import Debian.Debianize.Atoms (Atoms, Atom(..), newAtoms, InstallFile(..), Server(..), Site(..), DebInfo, debInfo, atomSet, makeDebInfo)
-- import Debian.Debianize.Monad (execCabalT, evalCabalT, CabalT, liftCabal, execDebianT)
-- import Debian.Debianize.SourceDebDescription (SourceDebDescription)
-- import Debian.Debianize.Output (compareDebianization)
-- import Debian.Debianize.Prelude ((~=), (%=), (+=), (++=), (+++=), (~?=), withCurrentDirectory)
import Debian.Pretty (ppShow)
import Debian.Policy (databaseDirectory, PackageArchitectures(All), StandardsVersion(StandardsVersion))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), SrcPkgName(..), VersionReq(SLT))
import Debian.Version (parseDebianVersion)

-- This looks somewhat like a "real" Debianize.hs file except that (1) it
-- expects to be run from the cabal-debian source directory and (2) it returns
-- the comparison string instead of doing a writeDebianization, and (3) it reads
-- and writes the test-data directories instead of ".".  Also, you wouldn't want
-- to copyFirstLogEntry in real life, this is to make sure old and new match.
main :: IO ()
main =
    do log <- withCurrentDirectory "test-data/artvaluereport2/input" $ newFlags >>= newCabalInfo >>= evalCabalT (liftCabal inputChangeLog >> access (debInfo . changelog))
       new <- withCurrentDirectory "test-data/artvaluereport2/input" $ newFlags >>= newCabalInfo >>= execCabalT (debianize (debianDefaults >> customize log {- >> removeFirstLogEntry -}))
       old <- withCurrentDirectory "test-data/artvaluereport2/output" $ newFlags >>= execDebianT inputDebianization . makeDebInfo
       -- The newest log entry gets modified when the Debianization is
       -- generated, it won't match so drop it for the comparison.
       compareDebianization old ({-copyFirstLogEntry old $ -} getL debInfo new) >>= putStr
    where
      customize :: Maybe ChangeLog -> CabalT IO ()
      customize log =
          do (debInfo . revision) ~= Nothing
             (debInfo . sourceFormat) ~= Just Native3
             (debInfo . changelog) ~?= log
             (debInfo . atomSet) %= (Set.insert $ InstallCabalExec (BinPkgName "appraisalscope") "lookatareport" "usr/bin")
             doExecutable (BinPkgName "appraisalscope") (InstallFile {execName = "appraisalscope", sourceDir = Nothing, destDir = Nothing, destName = "appraisalscope"})
             doServer (BinPkgName "artvaluereport2-development") (theServer (BinPkgName "artvaluereport2-development"))
             doServer (BinPkgName "artvaluereport2-staging") (theServer (BinPkgName "artvaluereport2-staging"))
             doWebsite (BinPkgName "artvaluereport2-production") (theSite (BinPkgName "artvaluereport2-production"))
             doBackups (BinPkgName "artvaluereport2-backups") "artvaluereport2-backups"
             -- This should go into the "real" data directory.  And maybe a different icon for each server?
             -- install (BinPkgName "artvaluereport2-server") ("theme/ArtValueReport_SunsetSpectrum.ico", "usr/share/artvaluereport2-data")
             (debInfo . binaryDebDescription (BinPkgName "artvaluereport2-backups") . description) ~=
                     Just (Text.intercalate "\n"
                                  [ "backup program for the appraisalreportonline.com site"
                                  , "  Install this somewhere other than where the server is running get"
                                  , "  automated backups of the database." ])
             addDep (BinPkgName "artvaluereport2-production") (BinPkgName "apache2")
             addServerData
             addServerDeps
             (debInfo . binaryDebDescription (BinPkgName "appraisalscope") . description) ~= Just "Offline manipulation of appraisal database"
             (debInfo . control . buildDependsIndep) %= (++ [[Rel (BinPkgName "libjs-jquery-ui") (Just (SLT (parseDebianVersion ("1.10" :: String)))) Nothing]])
             (debInfo . control . buildDependsIndep) %= (++ [[Rel (BinPkgName "libjs-jquery") Nothing Nothing]])
             (debInfo . control . buildDependsIndep) %= (++ [[Rel (BinPkgName "libjs-jcrop") Nothing Nothing]])
             (debInfo . binaryDebDescription (BinPkgName "artvaluereport2-staging") . architecture) ~= Just All
             (debInfo . binaryDebDescription (BinPkgName "artvaluereport2-production") . architecture) ~= Just All
             (debInfo . binaryDebDescription (BinPkgName "artvaluereport2-development") . architecture) ~= Just All
             -- utilsPackageNames [BinPkgName "artvaluereport2-server"]
             (debInfo . sourcePackageName) ~= Just (SrcPkgName "haskell-artvaluereport2")
             (debInfo . control . standardsVersion) ~= Just (StandardsVersion 3 9 6 Nothing)
             (debInfo . control . homepage) ~= Just "http://appraisalreportonline.com"
             (debInfo . compat) ~= Just 9

      addServerDeps :: CabalT IO ()
      addServerDeps = mapM_ addDeps (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addDeps p = mapM_ (addDep p) (map BinPkgName ["libjpeg-progs", "libjs-jcrop", "libjs-jquery", "libjs-jquery-ui", "netpbm", "texlive-fonts-extra", "texlive-fonts-recommended", "texlive-latex-extra", "texlive-latex-recommended"])
      addDep p dep = (debInfo . binaryDebDescription p . relations . depends) %= (++ [[Rel dep Nothing Nothing]])

      addServerData :: CabalT IO ()
      addServerData = mapM_ addData (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addData p =
          do (debInfo . atomSet) %= (Set.insert $ InstallData p "theme/ArtValueReport_SunsetSpectrum.ico" "ArtValueReport_SunsetSpectrum.ico")
             mapM_ (addDataFile p) ["Udon.js", "flexbox.css", "DataTables-1.8.2", "html5sortable", "jGFeed", "searchMag.png",
                                    "Clouds.jpg", "tweaks.css", "verticalTabs.css", "blueprint", "jquery.blockUI", "jquery.tinyscrollbar"]
      addDataFile p path = (debInfo . atomSet) %= (Set.insert $ InstallData p path path)

      theSite :: BinPkgName -> Site
      theSite deb =
          Site { domain = hostname'
               , serverAdmin = "logic@seereason.com"
               , server = theServer deb }
      theServer :: BinPkgName -> Server
      theServer deb =
          Server { hostname =
                       case deb of
                         BinPkgName "artvaluereport2-production" -> hostname'
                         _ -> hostname'
                 , port = portNum deb
                 , headerMessage = "Generated by artvaluereport2/Setup.hs"
                 , retry = "60"
                 , serverFlags =
                    ([ "--http-port", show (portNum deb)
                     , "--base-uri", case deb of
                                       BinPkgName "artvaluereport2-production" -> "http://" ++ hostname' ++ "/"
                                       _ -> "http://seereason.com:" ++ show (portNum deb) ++ "/"
                     , "--top", databaseDirectory deb
                     , "--logs", "/var/log/" ++ ppShow deb
                     , "--log-mode", case deb of
                                       BinPkgName "artvaluereport2-production" -> "Production"
                                       _ -> "Development"
                     , "--static", "/usr/share/artvaluereport2-data"
                     , "--no-validate" ] ++
                     (case deb of
                        BinPkgName "artvaluereport2-production" -> [{-"--enable-analytics"-}]
                        _ -> []) {- ++
                     [ "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", jstreePath
                     , "--json2-path",json2Path ] -})
                 , installFile =
                     InstallFile { execName   = "artvaluereport2-server"
                                 , destName   = ppShow deb
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }
      hostname' = "my.appraisalreportonline.com"
      portNum :: BinPkgName -> Int
      portNum (BinPkgName deb) =
          case deb of
            "artvaluereport2-production"  -> 9027
            "artvaluereport2-staging"     -> 9031
            "artvaluereport2-development" -> 9032
            _ -> error $ "Unexpected package name: " ++ deb

anyrel :: BinPkgName -> Relation
anyrel b = Rel b Nothing Nothing

removeFirstLogEntry :: Monad m => CabalT m ()
removeFirstLogEntry = (debInfo . changelog) %= fmap (\ (ChangeLog (_ : tl)) -> ChangeLog tl)

copyFirstLogEntry :: DebInfo -> DebInfo -> DebInfo
copyFirstLogEntry deb1 deb2 =
    modL changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL changelog deb2)
