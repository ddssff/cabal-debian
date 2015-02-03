{-# LANGUAGE CPP, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Support for generating Debianization from Cabal data.

module Debian.Debianize.SubstVars
    ( substvars
    ) where

import Control.Category ((.))
import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State (get)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Lens.Lazy (access, getL, modL)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, partition, unlines)
import Data.List as List (map)
import qualified Data.Map as Map (insert, lookup, Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set (member, Set)
import Data.Text (pack)
import Debian.Control (Control'(unControl), ControlFunctions(lookupP, parseControl, stripWS), Field'(Field))
import qualified Debian.Debianize.DebInfo as D (flags, extraLibMap, missingDependencies)
import Debian.Debianize.InputCabalPackageDescription (DebType(..), dryRun)
import Debian.Debianize.Monad (CabalT)
import Debian.Debianize.Prelude ((!), buildDebVersionMap, cond, DebMap, debOfFile, diffFile, dpkgFileMap, modifyM, replaceFile, showDeps)
import qualified Debian.Debianize.Atoms as A (Atoms, debInfo, packageDescription, packageInfo, PackageInfo(PackageInfo, cabalName, devDeb, docDeb, profDeb))
import Debian.Orphans ()
import Debian.Pretty (ppDisplay)
import Debian.Relation (BinPkgName(BinPkgName), Relation, Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), ParseRelations(parseRelations), Relation(Rel), Relations, VersionReq(GRE))
import Distribution.Compiler (buildCompilerId)
import Distribution.Package (Dependency(..), PackageName(PackageName))
import Distribution.PackageDescription as Cabal (allBuildInfo, buildTools, extraLibs, PackageDescription(..), pkgconfigDepends)
import Distribution.Simple.Utils (die)
import Distribution.Text (display)
import Prelude hiding ((.), unlines)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- | Expand the contents of the .substvars file for a library package.
-- Each cabal package corresponds to a directory <name>-<version>,
-- either in /usr/lib or in /usr/lib/haskell-packages/ghc/lib.  In
-- that directory is a compiler subdirectory such as ghc-6.8.2.  In
-- the ghc subdirectory is one or two library files of the form
-- libHS<name>-<version>.a and libHS<name>-<version>_p.a.  We can
-- determine the debian package names by running dpkg -S on these
-- names, or examining the /var/lib/dpkg/info/\*.list files.  From
-- these we can determine the source package name, and from that the
-- documentation package name.
substvars :: (MonadIO m, Functor m) =>
             DebType  -- ^ The type of deb we want to write substvars for - Dev, Prof, or Doc
          -> CabalT m ()
substvars debType =
    do debVersions <- liftIO buildDebVersionMap
       modifyM (liftIO . libPaths debVersions)
       control <- liftIO $ readFile "debian/control" >>= either (error . show) return . parseControl "debian/control"
       substvars' debType control

substvars' :: (MonadIO m, Functor m) => DebType -> Control' String -> CabalT m ()
substvars' debType control =
    get >>= return . getL A.packageInfo >>= \ info ->
    cabalDependencies >>= \ cabalDeps ->
    case (missingBuildDeps info cabalDeps, path) of
      -- There should already be a .substvars file produced by dh_haskell_prep,
      -- keep the relations listed there.  They will contain something like this:
      -- libghc-cabal-debian-prof.substvars:
      --    haskell:Depends=ghc-prof (<< 6.8.2-999), ghc-prof (>= 6.8.2), libghc-cabal-debian-dev (= 0.4)
      -- libghc-cabal-debian-dev.substvars:
      --    haskell:Depends=ghc (<< 6.8.2-999), ghc (>= 6.8.2)
      -- haskell-cabal-debian-doc.substvars:
      --    haskell:Depends=ghc-doc, haddock (>= 2.1.0), haddock (<< 2.1.0-999)
      ([], Just path') ->
          do old <- liftIO $ readFile path'
             deps <- debDeps debType control
             new <- addDeps old deps
             dry <- get >>= return . getL (dryRun . D.flags . A.debInfo)
             liftIO (diffFile path' (pack new) >>= maybe (putStrLn ("cabal-debian substvars: No updates found for " ++ show path'))
                                                       (\ diff -> if dry then putStr diff else replaceFile path' new))
      ([], Nothing) -> return ()
      (missing, _) ->
          liftIO $ die ("These debian packages need to be added to the build dependency list so the required cabal " ++
                        "packages are available:\n  " ++ intercalate "\n  " (map (ppDisplay . fst) missing) ++
                        "\nIf this is an obsolete package you may need to withdraw the old versions from the\n" ++
                        "upstream repository, and uninstall and purge it from your local system.")
    where
      addDeps old deps =
          case partition (isPrefixOf "haskell:Depends=") (lines old) of
            ([], other) -> filterMissing deps >>= \ deps' -> return $ unlines (("haskell:Depends=" ++ showDeps deps') : other)
            (hdeps, more) ->
                case deps of
                  [] -> return $ unlines (hdeps ++ more)
                  _ -> filterMissing deps >>= \ deps' -> return $ unlines (map (++ (", " ++ showDeps deps')) hdeps ++ more)
      path = fmap (\ (D.BinPkgName x) -> "debian/" ++ x ++ ".substvars") name
      name = debNameFromType control debType
      -- We must have build dependencies on the profiling and documentation packages
      -- of all the cabal packages.
      missingBuildDeps info cabalDeps =
          let requiredDebs =
                  concat (map (\ (Dependency name _) ->
                               case Map.lookup name info of
                                 Just info' ->
                                     let prof = maybe (A.devDeb info') Just (A.profDeb info') in
                                     let doc = A.docDeb info' in
                                     catMaybes [prof, doc]
                                 Nothing -> []) cabalDeps) in
          filter (not . (`elem` buildDepNames) . fst) requiredDebs
      buildDepNames :: [D.BinPkgName]
      buildDepNames = concat (map (map (\ (D.Rel s _ _) -> s)) buildDeps)
      buildDeps :: D.Relations
      buildDeps = (either (error . show) id . D.parseRelations $ bd) ++ (either (error . show) id . D.parseRelations $ bdi)
      --sourceName = maybe (error "Invalid control file") (\ (Field (_, s)) -> stripWS s) (lookupP "Source" (head (unControl control)))
      bd = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends" . head . unControl $ control
      bdi = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends-Indep" . head . unControl $ control

libPaths :: DebMap -> A.Atoms -> IO A.Atoms
libPaths debVersions atoms =
    do a <- getDirPaths "/usr/lib"
       b <- getDirPaths ("/usr/lib/haskell-packages" </> display buildCompilerId </> "lib")
       -- Build a map from names of installed debs to version numbers
       dpkgFileMap >>= runReaderT (foldM (packageInfo' debVersions) atoms (a ++ b))
    where
      getDirPaths path = try (getDirectoryContents path) >>= return . map (\ x -> (path, x)) . either (\ (_ :: SomeException) -> []) id

packageInfo' :: DebMap -> A.Atoms -> (FilePath, String) -> ReaderT (Map.Map FilePath (Set.Set D.BinPkgName)) IO A.Atoms
packageInfo' debVersions atoms (d, f) =
    case parseNameVersion f of
      Nothing -> return atoms
      Just (p, v) -> lift (doesDirectoryExist (d </> f </> cdir)) >>= cond (return atoms) (info (p, v))
    where
      parseNameVersion s =
          case (break (== '-') (reverse s)) of
            (_a, "") -> Nothing
            (a, b) -> Just (reverse (tail b), reverse a)
      cdir = display buildCompilerId
      info (p, v) =
          do dev <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ ".a$")
             prof <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ "_p.a$")
             doc <- debOfFile ("/" ++ p ++ ".haddock$")
             return $ modL A.packageInfo (Map.insert
                                           (PackageName p)
                                           (A.PackageInfo { A.cabalName = PackageName p
                                                          , A.devDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) dev
                                                          , A.profDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) prof
                                                          , A.docDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) doc })) atoms

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- Make a list of the debian devel packages corresponding to cabal packages
-- which are build dependencies
debDeps :: (MonadIO m, Functor m) => DebType -> Control' String -> CabalT m D.Relations
debDeps debType control =
    do info <- get >>= return . getL A.packageInfo
       cabalDeps <- cabalDependencies
       return $ interdependencies ++ otherdependencies info cabalDeps
    where
      interdependencies =
          case debType of
            Prof -> maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) (debNameFromType control Dev)
            _ -> []
      otherdependencies info cabalDeps =
          catMaybes (map (\ (Dependency name _) ->
                          case Map.lookup name info of
                            Just p -> maybe Nothing
                                            (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing])
                                            (case debType of
                                               Dev -> A.devDeb p
                                               Prof -> A.profDeb p
                                               Doc -> A.docDeb p)
                            Nothing -> Nothing) cabalDeps)

cabalDependencies :: (MonadIO m, Functor m) => CabalT m [Dependency]
cabalDependencies =
    do pkgDesc <- access A.packageDescription
       atoms <- get
       return $ catMaybes $ map unboxDependency $
           allBuildDepends atoms
                  (Cabal.buildDepends pkgDesc)
                  (concatMap buildTools . allBuildInfo $ pkgDesc)
                  (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                  (concatMap extraLibs . allBuildInfo $ pkgDesc)

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: A.Atoms -> [Dependency] -> [Dependency] -> [Dependency] -> [String] -> [Dependency_]
allBuildDepends atoms buildDepends buildTools pkgconfigDepends extraLibs =
    nub $ map BuildDepends buildDepends ++
          map BuildTools buildTools ++
          map PkgConfigDepends pkgconfigDepends ++
          map ExtraLibs (fixDeps extraLibs)
    where
      fixDeps :: [String] -> [Relations]
      fixDeps xs = map (\ cab -> fromMaybe [[D.Rel (D.BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]
                                           (Map.lookup cab (getL (D.extraLibMap . A.debInfo) atoms))) xs

-- | Given a control file and a DebType, look for the binary deb with
-- the corresponding suffix and return its name.
debNameFromType :: Control' String -> DebType -> Maybe BinPkgName
debNameFromType control debType =
    case debType of
      Dev -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-dev") debNames)
      Prof -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-prof") debNames)
      Doc -> fmap BinPkgName $ listToMaybe (filter (isSuffixOf "-doc") debNames)
    where
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))

filterMissing :: Monad m => [[Relation]] -> CabalT m [[Relation]]
filterMissing rels =
    do missing <- get >>= return . getL (D.missingDependencies . A.debInfo)
       return $ filter (/= []) (List.map (filter (\ (D.Rel name _ _) -> not (Set.member name missing))) rels)
