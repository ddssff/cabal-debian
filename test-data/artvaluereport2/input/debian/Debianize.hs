{-# LANGUAGE CPP, OverloadedStrings #-}
import Data.Lens.Lazy (getL, modL, access)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Set (singleton)
import Data.Text as Text (intercalate)
import Debian.Changes (ChangeLog(..))
import Debian.Debianize (debianization, doBackups, doExecutable, doServer, doWebsite, inputChangeLog, inputDebianization, seereasonDefaultAtoms)
import Debian.Debianize.Types as T
    (changelog, binaryArchitectures, buildDependsIndep, changelog, compat, control, depends, debianDescription,
     installCabalExec, installData, sourcePackageName, homepage, standardsVersion)
import Debian.Debianize.Types.Atoms as T
    (Atoms, newAtoms, InstallFile(..), Server(..), Site(..))
import Debian.Debianize.Monad (execDebT, evalDebT, DebT, execDebM)
import Debian.Debianize.Types (Top(Top))
import Debian.Debianize.Types.SourceDebDescription (SourceDebDescription)
import Debian.Debianize.Output (compareDebianization)
import Debian.Debianize.Prelude ((~=), (%=), (+=), (++=), (+++=), (~?=))
import Debian.Policy (databaseDirectory, PackageArchitectures(All), StandardsVersion(StandardsVersion))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel), SrcPkgName(..), VersionReq(SLT))
import Debian.Version (parseDebianVersion)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- This looks just like a "real" Debianize.hs file except that it
-- returns the comparison string instead of doing a
-- writeDebianization, and it reads and writes the test-data
-- directories instead of ".".  Also, you wouldn't want to
-- copyFirstLogEntry.
main :: IO ()
main =
    do log <- evalDebT (inputChangeLog (Top "test-data/artvaluereport2/input") >> access changelog) newAtoms
       new <- execDebT (debianization (Top "test-data/artvaluereport2/input") seereasonDefaultAtoms (customize log)) newAtoms
       old <- execDebT (inputDebianization (Top "test-data/artvaluereport2/output")) newAtoms
       -- The newest log entry gets modified when the Debianization is
       -- generated, it won't match so drop it for the comparison.
       compareDebianization old (copyFirstLogEntry old new) >>= putStr
    where
      customize :: Maybe ChangeLog -> DebT IO ()
      customize log =
          do T.changelog ~?= log
             installCabalExec +++= (BinPkgName "appraisalscope", singleton ("lookatareport", "usr/bin"))
             doExecutable (BinPkgName "appraisalscope") (InstallFile {execName = "appraisalscope", sourceDir = Nothing, destDir = Nothing, destName = "appraisalscope"})
             doServer (BinPkgName "artvaluereport2-development") (theServer (BinPkgName "artvaluereport2-development"))
             doServer (BinPkgName "artvaluereport2-staging") (theServer (BinPkgName "artvaluereport2-staging"))
             doWebsite (BinPkgName "artvaluereport2-production") (theSite (BinPkgName "artvaluereport2-production"))
             doBackups (BinPkgName "artvaluereport2-backups") "artvaluereport2-backups"
             -- This should go into the "real" data directory.  And maybe a different icon for each server?
             -- install (BinPkgName "artvaluereport2-server") ("theme/ArtValueReport_SunsetSpectrum.ico", "usr/share/artvaluereport2-data")
             debianDescription (BinPkgName "artvaluereport2-backups") ~=
                     Just (Text.intercalate "\n"
                                  [ "backup program for the appraisalreportonline.com site"
                                  , "  Install this somewhere other than where the server is running get"
                                  , "  automated backups of the database." ])
             addDep (BinPkgName "artvaluereport2-production") (BinPkgName "apache2")
             addServerData
             addServerDeps
             debianDescription (BinPkgName "appraisalscope") ~= Just "Offline manipulation of appraisal database"
             buildDependsIndep %= (++ [[Rel (BinPkgName "libjs-jquery-ui") (Just (SLT (parseDebianVersion ("1.10" :: String)))) Nothing]])
             buildDependsIndep %= (++ [[Rel (BinPkgName "libjs-jquery") Nothing Nothing]])
             buildDependsIndep %= (++ [[Rel (BinPkgName "libjs-jcrop") Nothing Nothing]])
             binaryArchitectures (BinPkgName "artvaluereport2-staging") ~= Just All
             binaryArchitectures (BinPkgName "artvaluereport2-production") ~= Just All
             binaryArchitectures (BinPkgName "artvaluereport2-development") ~= Just All
             -- utilsPackageNames [BinPkgName "artvaluereport2-server"]
             sourcePackageName ~= Just (SrcPkgName "haskell-artvaluereport2")
             T.standardsVersion ~= Just (StandardsVersion 3 9 1 Nothing)
             homepage ~= Just "http://appraisalreportonline.com"
             compat ~= Just 7

      addServerDeps :: DebT IO ()
      addServerDeps = mapM_ addDeps (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addDeps p = mapM_ (addDep p) (map BinPkgName ["libjpeg-progs", "libjs-jcrop", "libjs-jquery", "libjs-jquery-ui", "netpbm", "texlive-fonts-extra", "texlive-fonts-recommended", "texlive-latex-extra", "texlive-latex-recommended"])
      addDep p dep = depends p %= (++ [[Rel dep Nothing Nothing]])

      addServerData :: DebT IO ()
      addServerData = mapM_ addData (map BinPkgName ["artvaluereport2-development", "artvaluereport2-staging", "artvaluereport2-production"])
      addData p =
          do installData +++= (p, singleton ("theme/ArtValueReport_SunsetSpectrum.ico", "ArtValueReport_SunsetSpectrum.ico"))
             mapM_ (addDataFile p) ["Udon.js", "flexbox.css", "DataTables-1.8.2", "html5sortable", "jGFeed", "searchMag.png",
                                    "Clouds.jpg", "tweaks.css", "verticalTabs.css", "blueprint", "jquery.blockUI", "jquery.tinyscrollbar"]
      addDataFile p path = installData +++= (p, singleton (path, path))

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
                     , "--logs", "/var/log/" ++ show (pretty deb)
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
                                 , destName   = show (pretty deb)
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

copyFirstLogEntry :: Atoms -> Atoms -> Atoms
copyFirstLogEntry deb1 deb2 =
    modL T.changelog (const (Just (ChangeLog (hd1 : tl2)))) deb2
    where
      ChangeLog (hd1 : _) = fromMaybe (error "Missing debian/changelog") (getL T.changelog deb1)
      ChangeLog (_ : tl2) = fromMaybe (error "Missing debian/changelog") (getL T.changelog deb2)
