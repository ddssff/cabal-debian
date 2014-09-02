-- | Things that seem like they could be clients of this library, but
-- are instead included as part of the library.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Debian.Debianize.Goodies
    ( tightDependencyFixup
    , doServer
    , doWebsite
    , doBackups
    , doExecutable
    , describe
    , watchAtom
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    , siteAtoms
    , serverAtoms
    , backupAtoms
    , execAtoms
    , makeRulesHead
    ) where

import Data.Char (toLower)
import Data.Lens.Lazy (modL, access)
import Data.List as List (map, intersperse, intercalate)
import Data.Map as Map (insertWith)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (insert, union, singleton, toList)
import Data.Text as Text (Text, pack, unlines, intercalate)
import Debian.Debianize.DebianName (debianNameBase)
import Debian.Debianize.Monad (Atoms, DebT, execDebM)
import Debian.Debianize.Prelude (trim, (%=), (+=), (++=), (+++=))
import qualified Debian.Debianize.Types as T
import qualified Debian.Debianize.Types.Atoms as T
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Orphans ()
import Debian.Pretty (Pretty(pretty))
import Debian.Policy (apacheLogDirectory, apacheErrorLog, apacheAccessLog, databaseDirectory, serverAppLog, serverAccessLog)
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Package (PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (PackageDescription(package, synopsis, description, author, maintainer, pkgUrl))
import Distribution.Text (display)
import Prelude hiding (writeFile, init, unlines, log, map)
import System.FilePath ((</>))

showCommand :: String -> [String] -> String
showCommand cmd args =
    unwords (map translate (cmd : args))

translate :: String -> String
translate str =
    '"' : foldr escape "\"" str
    where
      escape '"' = showString "\\\""
      escape c = showChar c

-- | Create equals dependencies.  For each pair (A, B), use dpkg-query
-- to find out B's version number, version B.  Then write a rule into
-- P's .substvar that makes P require that that exact version of A,
-- and another that makes P conflict with any older version of A.
tightDependencyFixup :: Monad m => [(BinPkgName, BinPkgName)] -> BinPkgName -> DebT m ()
tightDependencyFixup [] _ = return ()
tightDependencyFixup pairs p =
    T.rulesFragments +=
          (Text.unlines $
               ([ "binary-fixup/" <> name <> "::"
                , "\techo -n 'haskell:Depends=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map equals pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars"
                , "\techo -n 'haskell:Conflicts=' >> debian/" <> name <> ".substvars" ] ++
                intersperse ("\techo -n ', ' >> debian/" <> name <> ".substvars") (List.map newer pairs) ++
                [ "\techo '' >> debian/" <> name <> ".substvars" ]))
    where
      equals (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (=$${Version})' " <>  display' installed <> " >> debian/" <> name <> ".substvars"
      newer  (installed, dependent) = "\tdpkg-query -W -f='" <> display' dependent <> " (>>$${Version})' " <> display' installed <> " >> debian/" <> name <> ".substvars"
      name = display' p
      display' = pack . show . pretty

-- | Add a debian binary package to the debianization containing a cabal executable file.
doExecutable :: Monad m => BinPkgName -> T.InstallFile -> DebT m ()
doExecutable p f = T.executable ++= (p, f)

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a server.
doServer :: Monad m => BinPkgName -> T.Server -> DebT m ()
doServer p s = T.serverInfo ++= (p, s)

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a web site.
doWebsite :: Monad m => BinPkgName -> T.Site -> DebT m ()
doWebsite p w = T.website ++= (p, w)

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a backup script.
doBackups :: Monad m => BinPkgName -> String -> DebT m ()
doBackups bin s =
    do T.backups ++= (bin, s)
       T.depends bin %= (++ [[Rel (BinPkgName "anacron") Nothing Nothing]])
       -- depends +++= (bin, Rel (BinPkgName "anacron") Nothing Nothing)

describe :: Monad m => BinPkgName -> DebT m Text
describe b =
    do Just p <- access T.packageDescription
       typ <- access (T.packageType b)
       return $
          debianDescriptionBase p <> "\n" <>
          case typ of
            Just B.Profiling ->
                Text.intercalate "\n"
                        [" .",
                         " This package provides a library for the Haskell programming language, compiled",
                         " for profiling.  See http:///www.haskell.org/ for more information on Haskell."]
            Just B.Development ->
                Text.intercalate "\n"
                        [" .",
                         " This package provides a library for the Haskell programming language.",
                         " See http:///www.haskell.org/ for more information on Haskell."]
            Just B.Documentation ->
                Text.intercalate "\n"
                        [" .",
                         " This package provides the documentation for a library for the Haskell",
                         " programming language.",
                         " See http:///www.haskell.org/ for more information on Haskell." ]
            Just B.Exec ->
                Text.intercalate "\n"
                        [" .",
                         " An executable built from the " <> pack (display (pkgName (Cabal.package p))) <> " package."]
      {-    ServerPackage ->
                Text.intercalate "\n"
                        [" .",
                         " A server built from the " <> pack (display (pkgName pkgId)) <> " package."] -}
            _ {-Utilities-} ->
                Text.intercalate "\n"
                        [" .",
                         " Files associated with the " <> pack (display (pkgName (Cabal.package p))) <> " package."]
            -- x -> error $ "Unexpected library package name suffix: " ++ show x

-- | The Cabal package has one synopsis and one description field
-- for the entire package, while in a Debian package there is a
-- description field (of which the first line is synopsis) in
-- each binary package.  So the cabal description forms the base
-- of the debian description, each of which is amended.
debianDescriptionBase :: PackageDescription -> Text
debianDescriptionBase p =
    (pack . unwords . words $ Cabal.synopsis p) <>
    case Cabal.description p of
      "" -> ""
      text ->
          let text' = text ++ "\n" ++
                      list "" ("\n Author: " ++) (Cabal.author p) ++
                      list "" ("\n Upstream-Maintainer: " ++) (Cabal.maintainer p) ++
                      list "" ("\n Url: " ++) (Cabal.pkgUrl p) in
          "\n " <> (pack . trim . List.intercalate "\n " . List.map addDot . lines $ text')
    where
      addDot line = if all (flip elem " \t") line then "." else line
      list :: b -> ([a] -> b) -> [a] -> b
      list d f l = case l of [] -> d; _ -> f l

oldClckwrksSiteFlags :: T.Site -> [String]
oldClckwrksSiteFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ T.domain x ++ "/"
    , "--http-port", show T.port]
oldClckwrksServerFlags :: T.Server -> [String]
oldClckwrksServerFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ T.hostname x ++ ":" ++ show (T.port x) ++ "/"
    , "--http-port", show T.port]

watchAtom :: PackageName -> Text
watchAtom (PackageName pkgname) =
    pack $ "version=3\nhttp://hackage.haskell.org/package/" ++ pkgname ++ "/distro-monitor .*-([0-9\\.]+)\\.(?:zip|tgz|tbz|txz|(?:tar\\.(?:gz|bz2|xz)))\n"

-- FIXME - use Atoms
siteAtoms :: BinPkgName -> T.Site -> Atoms -> Atoms
siteAtoms b site =
    execDebM
      (do T.installDir b "/etc/apache2/sites-available"
          T.link b ("/etc/apache2/sites-available/" ++ T.domain site) ("/etc/apache2/sites-enabled/" ++ T.domain site)
          T.file b ("/etc/apache2/sites-available" </> T.domain site) apacheConfig
          T.installDir b (apacheLogDirectory b)
          T.logrotateStanza +++= (b, singleton
                                   (Text.unlines $ [ pack (apacheAccessLog b) <> " {"
                                                   , "  copytruncate" -- hslogger doesn't notice when the log is rotated, maybe this will help
                                                   , "  weekly"
                                                   , "  rotate 5"
                                                   , "  compress"
                                                   , "  missingok"
                                                   , "}"]))
          T.logrotateStanza +++= (b, singleton
                                   (Text.unlines $ [ pack (apacheErrorLog b) <> " {"
                                                   , "  copytruncate"
                                                   , "  weekly"
                                                   , "  rotate 5"
                                                   , "  compress"
                                                   , "  missingok"
                                                   , "}" ]))) .
      serverAtoms b (T.server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          Text.unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " <> pack (T.serverAdmin site)
                   , "    ServerName www." <> pack (T.domain site)
                   , "    ServerAlias " <> pack (T.domain site)
                   , ""
                   , "    ErrorLog " <> pack (apacheErrorLog b)
                   , "    CustomLog " <> pack (apacheAccessLog b) <> " combined"
                   , ""
                   , "    ProxyRequests Off"
                   , "    AllowEncodedSlashes NoDecode"
                   , ""
                   , "    <Proxy *>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                Deny from all"
                   , "                #Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    <Proxy http://127.0.0.1:" <> port' <> "/*>"
                   , "                AddDefaultCharset off"
                   , "                Order deny,allow"
                   , "                #Allow from .example.com"
                   , "                #Deny from all"
                   , "                Allow from all"
                   , "    </Proxy>"
                   , ""
                   , "    SetEnv proxy-sendcl 1"
                   , ""
                   , "    ProxyPass / http://127.0.0.1:" <> port' <> "/ nocanon"
                   , "    ProxyPassReverse / http://127.0.0.1:" <> port' <> "/"
                   , "</VirtualHost>" ]
      port' = pack (show (T.port (T.server site)))

-- FIXME - use Atoms
serverAtoms :: BinPkgName -> T.Server -> Bool -> Atoms -> Atoms
serverAtoms b server' isSite =
    modL T.postInst (insertWith (\ old new -> if old /= new then error ("serverAtoms: " ++ show old ++ " -> " ++ show new) else old) b debianPostinst) .
    modL T.installInit (Map.insertWith (\ old new -> if old /= new then error ("serverAtoms: " ++ show old ++ " -> " ++ show new) else old) b debianInit) .
    serverLogrotate' b .
    execAtoms b exec
    where
      exec = T.installFile server'
      debianInit =
          Text.unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , "test -f /etc/default/" <> pack (T.destName exec) <> " && . /etc/default/" <> pack (T.destName exec)
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" <> pack (T.destName exec) <> " || exit 0"
                   , "    log_begin_msg \"Starting " <> pack (T.destName exec) <> "...\""
                   , "    mkdir -p " <> pack (databaseDirectory b)
                   , "    " <> startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " <> pack (T.destName exec) <> "...\""
                   , "    " <> stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = pack $ showCommand "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ T.serverFlags server')
      stopCommand = pack $ showCommand "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ T.destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> T.destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if T.retry server' /= "" then ["--retry=" ++ T.retry server' ] else []

      debianPostinst =
          Text.unlines $
                   ([ "#!/bin/sh"
                    , ""
                    , "case \"$1\" in"
                    , "  configure)" ] ++
                    (if isSite
                     then [ "    # Apache won't start if this directory doesn't exist"
                          , "    mkdir -p " <> pack (apacheLogDirectory b)
                          , "    # Restart apache so it sees the new file in /etc/apache2/sites-enabled"
                          , "    /usr/sbin/a2enmod proxy"
                          , "    /usr/sbin/a2enmod proxy_http"
                          , "    service apache2 restart" ]
                     else []) ++
                    [ -- This gets done by the #DEBHELPER# code below.
                      {- "    service " <> pack (show (pretty b)) <> " start", -}
                      "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
-- FIXME - use Atoms
serverLogrotate' :: BinPkgName -> Atoms -> Atoms
serverLogrotate' b =
    modL T.logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]))) .
    modL T.logrotateStanza (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])))

-- FIXME - use Atoms
backupAtoms :: BinPkgName -> String -> Atoms -> Atoms
backupAtoms b name =
    modL T.postInst (insertWith (\ old new -> if old /= new then error $ "backupAtoms: " ++ show old ++ " -> " ++ show new else old) b
                 (Text.unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])) .
    execAtoms b (T.InstallFile { T.execName = name
                               , T.destName = name
                               , T.sourceDir = Nothing
                               , T.destDir = Just "/etc/cron.hourly" })

-- FIXME - use Atoms
execAtoms :: BinPkgName -> T.InstallFile -> Atoms -> Atoms
execAtoms b ifile r =
    modL T.rulesFragments (Set.insert (pack ("build" </> show (pretty b) ++ ":: build-ghc-stamp"))) .
    fileAtoms b ifile $
    r

-- FIXME - use Atoms
fileAtoms :: BinPkgName -> T.InstallFile -> Atoms -> Atoms
fileAtoms b installFile' r =
    fileAtoms' b (T.sourceDir installFile') (T.execName installFile') (T.destDir installFile') (T.destName installFile') r

-- FIXME - use Atoms
fileAtoms' :: BinPkgName -> Maybe FilePath -> String -> Maybe FilePath -> String -> Atoms -> Atoms
fileAtoms' b sourceDir' execName' destDir' destName' r =
    case (sourceDir', execName' == destName') of
      (Nothing, True) -> execDebM (T.installCabalExec b execName' d) r
      (Just s, True) -> execDebM (T.install b (s </> execName') d) r
      (Nothing, False) -> execDebM (T.installCabalExecTo b execName' (d </> destName')) r
      (Just s, False) -> execDebM (T.installTo b (s </> execName') (d </> destName')) r
    where
      d = fromMaybe "usr/bin" destDir'

-- | Build a suitable value for the head of the rules file.
makeRulesHead :: (Monad m, Functor m) => DebT m Text
makeRulesHead =
    do DebBase b <- debianNameBase
       compilers <- access T.compilerFlavors
       let ls = ["DEB_CABAL_PACKAGE = " <> pack b] ++
                -- If there is a single entry in the compiler list we
                -- can tell debian/rules what compiler to use,
                -- otherwise it has to figure this out from the
                -- package names in debian/control.  This is required
                -- to build a package without a library.
                (case toList compilers of
                   [x] -> ["DEB_DEFAULT_COMPILER = " <> pack (map toLower (show x))]
                   _ -> []) ++
                [""]
       return $
          Text.unlines $
            ["#!/usr/bin/make -f", ""] ++
            ls ++
            ["include /usr/share/cdbs/1/rules/debhelper.mk",
             "include /usr/share/cdbs/1/class/hlibrary.mk"]
