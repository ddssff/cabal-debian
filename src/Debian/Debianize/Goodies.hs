-- | Things that seem like they could be clients of this library, but
-- are instead included as part of the library.
{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Debian.Debianize.Goodies
    ( tightDependencyFixup
    , expandWebsite, doWebsite
    , expandServer, doServer
    , expandBackups, doBackups
    , doExecutable
    , oldClckwrksSiteFlags
    , oldClckwrksServerFlags
    , siteAtoms
    , logrotate
    , serverAtoms
    , backupAtoms
    , execAtoms
    ) where

import Control.Lens
import Control.Monad.State (MonadState(get), modify)
import Data.List as List ({-dropWhileEnd, intercalate,-} intersperse, map)
import Data.Map as Map (insert, insertWith, toList)
import Data.Monoid ((<>))
import Data.Set as Set (insert, singleton, union)
import Data.Text as Text (pack, {-Text,-} unlines)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.ExecAtoms (execAtoms)
import Debian.Debianize.Monad (CabalInfo, CabalT, DebianT, execCabalM)
--import Debian.Debianize.Prelude (stripWith)
import qualified Debian.Debianize.CabalInfo as A
import qualified Debian.Debianize.BinaryDebDescription as B
import Debian.Orphans ()
import Debian.Policy (apacheAccessLog, apacheErrorLog, apacheLogDirectory, databaseDirectory, dataDirectory, serverAccessLog, serverAppLog)
import Debian.Pretty (ppText)
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Prelude hiding (init, log, map, unlines, writeFile)
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
tightDependencyFixup :: Monad m => [(BinPkgName, BinPkgName)] -> BinPkgName -> DebianT m ()
tightDependencyFixup [] _ = return ()
tightDependencyFixup pairs p =
    D.rulesFragments %= Set.insert
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
      display' = ppText

-- | Add a debian binary package to the debianization containing a cabal executable file.
doExecutable :: Monad m => BinPkgName -> D.InstallFile -> CabalT m ()
doExecutable p f = (A.debInfo . D.executable) %= Map.insert p f

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a server.
doServer :: Monad m => BinPkgName -> D.Server -> CabalT m ()
doServer p s = (A.debInfo . D.serverInfo) %= Map.insert p s

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a web site.
doWebsite :: Monad m => BinPkgName -> D.Site -> CabalT m ()
doWebsite p w = (A.debInfo . D.website) %= Map.insert p w

-- | Add a debian binary package to the debianization containing a cabal executable file set up to be a backup script.
doBackups :: Monad m => BinPkgName -> String -> CabalT m ()
doBackups bin s =
    do (A.debInfo . D.backups) %= Map.insert bin s
       (A.debInfo . D.binaryDebDescription bin . B.relations . B.depends) %= (++ [[Rel (BinPkgName "anacron") Nothing Nothing]])
       -- depends +++= (bin, Rel (BinPkgName "anacron") Nothing Nothing)

oldClckwrksSiteFlags :: D.Site -> [String]
oldClckwrksSiteFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ D.domain x ++ "/"
    , "--http-port", show (D.port (D.server x))]
oldClckwrksServerFlags :: D.Server -> [String]
oldClckwrksServerFlags x =
    [ -- According to the happstack-server documentation this needs a trailing slash.
      "--base-uri", "http://" ++ D.hostname x ++ ":" ++ show (D.port x) ++ "/"
    , "--http-port", show (D.port x)]

siteAtoms :: PackageDescription -> BinPkgName -> D.Site -> CabalInfo -> CabalInfo
siteAtoms pkgDesc b site =
    execCabalM
      (do (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallDir b "/etc/apache2/sites-available")
          (A.debInfo . D.atomSet) %= (Set.insert $ D.Link b ("/etc/apache2/sites-available/" ++ D.domain site ++ ".conf") ("/etc/apache2/sites-enabled/" ++ D.domain site ++ ".conf"))
          (A.debInfo . D.atomSet) %= (Set.insert $ D.File b ("/etc/apache2/sites-available" </> D.domain site ++ ".conf") apacheConfig)
          (A.debInfo . D.atomSet) %= (Set.insert $ D.InstallDir b (apacheLogDirectory b))
          {-logrotate b-}) .
      serverAtoms pkgDesc b (D.server site) True
    where
      -- An apache site configuration file.  This is installed via a line
      -- in debianFiles.
      apacheConfig =
          Text.unlines $
                   [  "<VirtualHost *:80>"
                   , "    ServerAdmin " <> pack (D.serverAdmin site)
                   , "    ServerName www." <> pack (D.domain site)
                   , "    ServerAlias " <> pack (D.domain site)
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
      port' = pack (show (D.port (D.server site)))

-- | Install configuration files to do log rotation.  This does not
-- work well with the haskell logging library, so it is no longer
-- called in siteAtoms.
logrotate :: MonadState CabalInfo m => BinPkgName -> m ()
logrotate b = do
          (A.debInfo . D.logrotateStanza) %= Map.insertWith mappend b
                              (singleton
                                   (Text.unlines $ [ pack (apacheAccessLog b) <> " {"
                                                   , "  weekly"
                                                   , "  rotate 5"
                                                   , "  compress"
                                                   , "  missingok"
                                                   , "}"]))
          (A.debInfo . D.logrotateStanza) %= Map.insertWith mappend b
                              (singleton
                                   (Text.unlines $ [ pack (apacheErrorLog b) <> " {"
                                                   , "  weekly"
                                                   , "  rotate 5"
                                                   , "  compress"
                                                   , "  missingok"
                                                   , "}" ]))

serverAtoms :: PackageDescription -> BinPkgName -> D.Server -> Bool -> CabalInfo -> CabalInfo
serverAtoms pkgDesc b server' isSite =
    over (A.debInfo . D.postInst) (insertWith failOnMismatch b debianPostinst) .
    over (A.debInfo . D.installInit) (Map.insertWith failOnMismatch b debianInit) .
    serverLogrotate' b .
    execAtoms b exec
    where
      -- Combine two values (for insertWith) when there should only be
      -- one.  If it happens twice with different values we should
      -- really find out why.
      failOnMismatch old new = if old /= new then error ("serverAtoms: " ++ show old ++ " -> " ++ show new) else old
      exec = D.installFile server'
      debianInit =
          Text.unlines $
                   [ "#! /bin/sh -e"
                   , ""
                   , ". /lib/lsb/init-functions"
                   , "test -f /etc/default/" <> pack (D.destName exec) <> " && . /etc/default/" <> pack (D.destName exec)
                   , ""
                   , "case \"$1\" in"
                   , "  start)"
                   , "    test -x /usr/bin/" <> pack (D.destName exec) <> " || exit 0"
                   , "    log_begin_msg \"Starting " <> pack (D.destName exec) <> "...\""
                   , "    mkdir -p " <> pack (databaseDirectory b)
                   , "    export " <> pack (pkgPathEnvVar pkgDesc "datadir") <> "=" <> pack (dataDirectory pkgDesc)
                   , "    " <> startCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  stop)"
                   , "    log_begin_msg \"Stopping " <> pack (D.destName exec) <> "...\""
                   , "    " <> stopCommand
                   , "    log_end_msg $?"
                   , "    ;;"
                   , "  *)"
                   , "    log_success_msg \"Usage: ${0} {start|stop}\""
                   , "    exit 1"
                   , "esac"
                   , ""
                   , "exit 0" ]
      startCommand = pack $ showCommand "start-stop-daemon" (startOptions ++ commonOptions ++ ["--"] ++ D.serverFlags server')
      stopCommand = pack $ showCommand "start-stop-daemon" (stopOptions ++ commonOptions)
      commonOptions = ["--pidfile", "/var/run/" ++ D.destName exec]
      startOptions = ["--start", "-b", "--make-pidfile", "-d", databaseDirectory b, "--exec", "/usr/bin" </> D.destName exec]
      stopOptions = ["--stop", "--oknodo"] ++ if D.retry server' /= "" then ["--retry=" ++ D.retry server' ] else []

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
                      {- "    service " <> pack (show (pPrint b)) <> " start", -}
                      "    ;;"
                    , "esac"
                    , ""
                    , "#DEBHELPER#"
                    , ""
                    , "exit 0" ])

-- | A configuration file for the logrotate facility, installed via a line
-- in debianFiles.
serverLogrotate' :: BinPkgName -> CabalInfo -> CabalInfo
serverLogrotate' b =
    over (A.debInfo . D.logrotateStanza) (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAccessLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ]))) .
    over (A.debInfo . D.logrotateStanza) (insertWith Set.union b (singleton (Text.unlines $ [ pack (serverAppLog b) <> " {"
                                 , "  weekly"
                                 , "  rotate 5"
                                 , "  compress"
                                 , "  missingok"
                                 , "}" ])))

backupAtoms :: BinPkgName -> String -> CabalInfo -> CabalInfo
backupAtoms b name =
    over (A.debInfo . D.postInst) (insertWith (\ old new -> if old /= new then error $ "backupAtoms: " ++ show old ++ " -> " ++ show new else old) b
                 (Text.unlines $
                  [ "#!/bin/sh"
                  , ""
                  , "case \"$1\" in"
                  , "  configure)"
                  , "    " <> pack ("/etc/cron.hourly" </> name) <> " --initialize"
                  , "    ;;"
                  , "esac" ])) .
    execAtoms b (D.InstallFile { D.execName = name
                               , D.destName = name
                               , D.sourceDir = Nothing
                               , D.destDir = Just "/etc/cron.hourly" })

expandWebsite :: Monad m => CabalT m ()
expandWebsite =
    do mp <- get >>= return . view (A.debInfo . D.website)
       pkgDesc <- use A.packageDescription
       mapM_ (\ (b, site) -> modify (siteAtoms pkgDesc b site)) (Map.toList mp)

expandServer :: Monad m => CabalT m ()
expandServer =
    do mp <- get >>= return . view (A.debInfo . D.serverInfo)
       pkgDesc <- use A.packageDescription
       mapM_ (\ (b, x) -> modify (serverAtoms pkgDesc b x False)) (Map.toList mp)

expandBackups :: Monad m => CabalT m ()
expandBackups =
    do mp <- get >>= return . view (A.debInfo . D.backups)
       mapM_ (\ (b, name) -> modify (backupAtoms b name)) (Map.toList mp)
